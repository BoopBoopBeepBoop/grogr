package grogr.engine.client

import sttp.client3._
import sttp.client3.httpclient.HttpClientFutureBackend
// TODO: get a different executor
import scala.concurrent.ExecutionContext.Implicits.global

object PlatformDependent {
  lazy val defaultBackend = HttpClientFutureBackend()

  def executionContext = scala.concurrent.ExecutionContext.Implicits.global
}
