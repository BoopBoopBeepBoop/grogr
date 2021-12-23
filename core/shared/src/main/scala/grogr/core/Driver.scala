package grogr.core

import cats.data.EitherT
import cats.implicits.*
import grogr.core.Driver.Validation.MissingArgument

import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

trait Driver {
  def name: String
  def connect(
      args: Map[String, String])(
      using ExecutionContext
  ): Driver.ValidationResult[Connection]
}

object Driver {
  type ValidationResult[A] = EitherT[Future, Validation, A]
  trait Validation {
    def msg: String
  }
  object Validation {
    case class MissingArgument(argName: String) extends Validation {
      override def msg: String = s"Missing required argument: $argName"
    }
    case class URIParseFailure(uri: String) extends Validation {
      override def msg: String = s"Uri could not be parsed (only absolute URI's are accepted): $uri"
    }
    case class RemoteConnectionFailure(destination: String, e: Throwable) extends Validation {
      override def msg: String = s"Connection instantiation failed [$destination] with message [${e.toString}]"
    }
  }
}

trait Connection {
  def initialize(
      args: Map[String, String])(
      using ExecutionContext
  ): Driver.ValidationResult[Session]
}

trait Session {
  def id: UUID
  def schema: model.Schema
}

object Arguments {
  extension (m: Map[String, String])
    def arg(str: String)(using ExecutionContext): Driver.ValidationResult[String] = EitherT(Future {
      m.get(str).toRight(MissingArgument(str))
    })
}
