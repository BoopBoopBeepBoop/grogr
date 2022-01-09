package grogr.engine

import grogr.core.{Driver, Session}

import scala.concurrent.{ExecutionContext, Future}

trait Engine {
  def drivers: Seq[String]
  def connect(driver: String, args: Map[String, String])(using ExecutionContext): Future[Session]
}

class DefaultEngine(driverInstances: Seq[Driver]) extends Engine {

  def drivers: Seq[String] = driverInstances.map(_.name)

  def connect(driver: String, args: Map[String, String])(using ExecutionContext): Future[Session] = {
    val driverInstance =
      driverInstances
        .find(_.name == driver)
        .getOrElse(throw new IllegalArgumentException(s"Driver not found: $driver"))

    driverInstance.connect(args)
      .flatMap(c => c.initialize(args))
      .fold(
        failed => throw new RuntimeException(s"Failed to instantiate: ${failed.msg}"),
        identity
      )
  }
}
