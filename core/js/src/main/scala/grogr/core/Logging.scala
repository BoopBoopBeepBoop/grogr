package grogr.core

import scala.scalajs.js
import scalajs.js.DynamicImplicits.number2dynamic

trait Logging {
  val logger = new Logger(getClass.getName)
}

object Logging {
  private [core] val TRACE = 0
  private [core] val DEBUG = 1
  private [core] val INFO = 2
  private [core] val WARN = 3
  private [core] val ERROR = 4
}

class Logger(name: String) {

  def trace(msg: String)(implicit fn: sourcecode.FullName, line: sourcecode.Line): Unit = {
    if(Global.getLogLevel() >= Logging.TRACE) println(fmt(msg))
  }

  def debug(msg: String)(implicit fn: sourcecode.FullName, line: sourcecode.Line): Unit = {
    if(Global.getLogLevel() >= Logging.DEBUG) println(fmt(msg))
  }

  def info(msg: String)(implicit fn: sourcecode.FullName, line: sourcecode.Line): Unit = {
    if(Global.getLogLevel() >= Logging.INFO) println(fmt(msg))
  }

  def warn(msg: String)(implicit fn: sourcecode.FullName, line: sourcecode.Line): Unit = {
    if(Global.getLogLevel() >= Logging.WARN) println(fmt(msg))
  }

  def error(msg: String)(implicit fn: sourcecode.FullName, line: sourcecode.Line): Unit = {
    if(Global.getLogLevel() >= Logging.ERROR) println(fmt(msg))
  }

  private def fmt(str: String)(implicit fn: sourcecode.FullName, line: sourcecode.Line) = {
    s"${fn.value}:${line.value} - $str"
  }
}