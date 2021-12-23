package grogr.server.route

import grogr.engine.Engine
import grogr.server.api.{EngineDTO, Protocol, SessionCreateDTO, SessionDTO}
import upickle.default.{read, write}
import Protocol.*

import scala.concurrent.Await
import scala.concurrent.duration.*

class EngineV1Routes(engine: Engine)(using castor.Context, cask.Logger) extends cask.Routes {

  @cask.getJson("/v1/engine")
  def get() = {
    EngineDTO(drivers = engine.drivers)
  }

  @cask.postJson("/v1/engine/session")
  def connect(req: SessionCreateDTO) = {
    val f = engine.connect(req.driver, req.args)
    val session = Await.result(f, 60.seconds)
    SessionDTO(session.id, session.schema)
  }

  initialize()
}
