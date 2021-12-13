package grogr.core

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobal, JSGlobalScope, JSName}

@js.native
@JSGlobal("window")
object Window extends js.Object {
  var logLevel: Int = js.native
}

object Global extends js.Object {
  var logLevel: Int = 2

  def getLogLevel(): Int = {
    if (js.typeOf(logLevel) == "undefined") {
      if (js.typeOf(js.Dynamic.global.logLevel) == "undefined") 2
      else Window.logLevel
    } else logLevel
  }
}