package grogr.engine.client

import grogr.engine.client.Clickhouse.Result
import sttp.client3.{SttpBackend, *}
import sttp.model.Uri
import upickle.default.{macroRW, ReadWriter as RW}
import upickle.implicits.key
import upickle.default as upickle

import java.lang.Package
import scala.concurrent.{ExecutionContext, Future}

object Clickhouse extends App {
  def apply(base: Uri, backend: SttpBackend[Future, _])(using exec: ExecutionContext) = {
    new ClickhouseClient(base, backend)
  }
  def apply(base: Uri)(using exec: ExecutionContext) = {
    new ClickhouseClient(base, PlatformDependent.defaultBackend)
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

class ClickhouseClient(base: Uri, backend: SttpBackend[Future, _])(using exec: ExecutionContext) {
  import Clickhouse.Protocol.given

  def query(stmt: String) = {
    val req = basicRequest.get(uri"$base?query=$stmt")
    req.send(backend).map(_.body match {
      case Right(resp) => upickle.read[Result](resp)
      case Left(foo) => throw new RuntimeException(foo)
    })
  }
}


