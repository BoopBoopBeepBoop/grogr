package grogr.clickhouse

import grogr.core.Driver.Validation
import org.scalatest.funspec.AsyncFunSpecLike
import org.scalatest.matchers.should.Matchers
import cats.data.{EitherT, Nested}
import cats.implicits.*
import sttp.client3.*

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

class TestClickhouseDriver extends AsyncFunSpecLike with Matchers {
  val driver = new ClickhouseDriver()

  it("should connect") {
    driver
      .connect(Map("uri" -> "http://localhost:8123"))
      .toNested
      .value
      .map { _ shouldBe a[Right[_, _]] }
  }

  it("should return bad argument") {
    driver
      .connect(Map.empty)
      .toNested
      .value
      .map {
        case Left(pf: Validation.MissingArgument) =>
          pf.msg should include ("uri")
        case other => fail(s"Unexpected: $other")
      }
  }

  it("should return bad URI") {
    driver
      .connect(Map("uri" -> ""))
      .toNested
      .value
      .map {
        case Left(pf: Validation.RemoteConnectionFailure) =>
          pf.msg should include ("Exception when sending request: GET :?query=select+1+format+JSON")
        case other => fail(s"Unexpected: $other")
      }
  }

  it("should retrieve schema") {
    def connect =
      for {
        connection <- driver.connect(Map("uri" -> "http://localhost:8123"))
        session <- connection.initialize(Map.empty)
      } yield session.schema

    connect.toNested.value.collect {
      case Right(schema) => schema.tables.size shouldEqual 1
      case Left(other) => fail(other.toString)
    }
  }
}