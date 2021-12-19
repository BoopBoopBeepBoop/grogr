package grogr.clickhouse

import cats.data.{EitherT, Nested}
import cats.implicits.*
import grogr.clickhouse.Clickhouse.ColMeta
import grogr.core.Driver.Validation.{MissingArgument, URIParseFailure}
import grogr.core.{Connection, Driver, Session, model}
import grogr.core.Arguments.arg
import sttp.model.Uri

import scala.concurrent.{ExecutionContext, Future}


class ClickhouseDriver extends Driver {
  val name = "clickhouse"

  def connect(
      args: Map[String, String])(
      using ExecutionContext
  ): Driver.ValidationResult[Connection] = {

    def makeClient: Driver.ValidationResult[ClickhouseClient] =
      for {
        uriArg <- args.arg("uri")
        uri <- EitherT(Future(
          Uri.parse(uriArg)
            .left
            .map(URIParseFailure(_))))
      } yield {
        Clickhouse(uri)
      }

    for {
      client <- makeClient
      validatedClient <- client.test()
    } yield {
      new ClickhouseConnection(validatedClient)
    }
  }
}

class ClickhouseConnection(client: ClickhouseClient) extends Connection {
  def initialize(
      args: Map[String, String])(
      using ExecutionContext
  ): Driver.ValidationResult[Session] = {

    def getSchema = {
      client.query("show tables format JSON").flatMap { res =>
        val tableNames = res.data.map { _("name").asInstanceOf[String] }
        val tablesf = Future.sequence(tableNames.map { name =>
          client.query(s"describe table $name format JSON").map { tableResult =>
            model.Table(
              name = name,
              columns = tableResult.data.map { untyped =>
                model.Column(
                  name = untyped("name").asInstanceOf[String],
                  `type` = untyped("type").asInstanceOf[String],
                )
              }
            )
          }
        })

        // TODO: no relations stored in clickhouse, figure that out later
        tablesf.map { tables => model.Schema(tables, relations = Nil) }
      }
    }

    EitherT(getSchema.map(s => Right(new ClickhouseSession(s))))
  }
}

class ClickhouseSession(val schema: model.Schema) extends Session {
}
