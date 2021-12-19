package grogr.clickhouse

import grogr.core.Driver.Validation.RemoteConnectionFailure
import grogr.core.{Driver, Logging}
import sttp.client3.httpclient.HttpClientFutureBackend
import sttp.client3.{SttpBackend, *}
import sttp.model.Uri
import upickle.default.{macroRW, ReadWriter as RW}
import upickle.implicits.key
import upickle.default as upickle
import cats.data.{EitherT, Validated}
import cats.data.Validated.*
import grogr.clickhouse.Clickhouse.Result

import java.lang.Package
import scala.concurrent.{ExecutionContext, Future}

object Clickhouse extends App {
  def apply(
      base: Uri,
      backend: SttpBackend[Future, _] = HttpClientFutureBackend())(
      using exec: ExecutionContext
  ) = {
    new ClickhouseClient(base, backend)
  }

  case class Result(
      meta: Seq[ColMeta],
      data: Seq[Map[String, Any]],
      rows: Long,
      statistics: Statistics
  )

  case class ColMeta(name: String, `type`: String)
  case class Statistics(
      elapsed: Double,
      @key("rows_read") rowsRead: Long,
      @key("bytes_read") bytesRead: Long
  )

  // a uPickle protocol defining how to ser/de the above classes
  object Protocol {
    given resultRW: RW[Result] = macroRW
    given colMetaRW: RW[ColMeta] = macroRW
    given statisticsRW: RW[Statistics] = macroRW

    given mapRW: RW[Map[String, Any]] = {
      upickle.readwriter[ujson.Value].bimap[Map[String, Any]](
        x => ujson.Obj.from(x.map { case (k, v) => k -> (v match {
          case s: String => ujson.Str(s)
          case i: Int => ujson.Num(i)
          case d: Double => ujson.Num(d)
          case other => throw new UnsupportedOperationException(s"Type not supported: $other")
        })}),
        (json: ujson.Value) => json match {
          case ujson.Obj(values) =>
            values.map { case (k, v) => k -> (v match {
              case ujson.Str(str) => str
              case ujson.Num(num) => num
              case other => throw new UnsupportedOperationException(s"Type not supported: $other")
            })}.toMap
          case other => throw new UnsupportedOperationException(s"Json not map: $other")
        }
      )
    }
  }

}

class ClickhouseClient(base: Uri, backend: SttpBackend[Future, _])(using exec: ExecutionContext) extends Logging {
  import Clickhouse.Protocol.given

  def test(): Driver.ValidationResult[ClickhouseClient] = {
    EitherT(
      query("select 1 format JSON")
        .map(_ => Right(this))
        .recover { case e => Left(RemoteConnectionFailure(base.toString, e)) })
  }

  def query(stmt: String): Future[Result] = {
    val req = basicRequest.get(uri"$base?query=$stmt")
    req.send(backend).map(_.body match {
      case Right(resp) => upickle.read[Result](resp)
      case Left(foo) => throw new RuntimeException(foo)
    })
  }
}


