package grogr.engine.client

import sttp.client3.FetchBackend

object PlatformDependent {
  lazy val defaultBackend = FetchBackend()

  def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
}
