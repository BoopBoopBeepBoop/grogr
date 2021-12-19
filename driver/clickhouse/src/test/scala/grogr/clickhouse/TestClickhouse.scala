package grogr.clickhouse

import org.scalatest.funspec.AsyncFunSpecLike
import org.scalatest.matchers.should.Matchers
import sttp.client3.*

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

class TestClickhouse extends AsyncFunSpecLike with Matchers {
  val client = Clickhouse(uri"http://localhost:8123")

  it("should query [select 1]") {
    client.query("select 1 format JSON").map { res =>
      res.data(0) shouldEqual (Map("1" -> 1.0))
    }
  }

  it("should test") {
    client.test().foldF(
      f => fail(f.msg),
      _ shouldEqual client
    )
  }

  it("should query [show tables]") {
    client.query("show tables format JSON").map { res =>
      res.data(0)("name") shouldEqual "TMP"
    }
  }

  it("should query [describe table]") {
    client.query("describe table TMP format JSON").map { res =>
      println(res)
      succeed
    }
  }
}