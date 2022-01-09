package grogr.server

import grogr.clickhouse.ClickhouseDriver
import grogr.engine.DefaultEngine
import grogr.server.route.*

object Application extends cask.Main {

  val drivers = Seq(
    new ClickhouseDriver()
  )

  val engine = new DefaultEngine(drivers)

  val allRoutes = Seq(
    new MinimalRoutes(),
    new EngineV1Routes(engine)
  )
}
