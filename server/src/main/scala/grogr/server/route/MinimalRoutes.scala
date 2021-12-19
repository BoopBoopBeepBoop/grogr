package grogr.server.route

class MinimalRoutes()(
    implicit cc: castor.Context,
    log: cask.Logger
) extends cask.Routes {

  @cask.get("/")
  def hello() = {
    "Hello World!"
  }

  @cask.post("/do-thing")
  def doThing(request: cask.Request) = {
    request.text().reverse
  }

  initialize()
}
