package grogr.server

object Application extends cask.Main {
  val allRoutes = Seq(
    new route.MinimalRoutes()
  )
}
