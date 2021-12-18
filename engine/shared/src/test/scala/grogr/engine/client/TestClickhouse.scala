package grogr.engine.client

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import sttp.client3.*

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class TestClickhouse extends AnyFunSpec with Matchers {

  val client = Clickhouse(uri"http://localhost:8123")

  it("should query [select 1]") {
    val f = client.query("select 1 format JSON")
    val resp = Await.result(f, 5.seconds)
    println(resp)
  }

  it("should query [show tables]") {
    val f = client.query("show tables format JSON")
    val resp = Await.result(f, 5.seconds)
    println(resp)
  }

  it("should query [describe table]") {
    val f = client.query("describe table TMP format JSON")
    val resp = Await.result(f, 5.seconds)
    println(resp)
  }
}