import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt._

object Dependencies {
  val caskVersion = "0.8.0"
  val catsVersion = "2.7.0"
  val janinoVersion = "3.1.6"
  val logbackVersion = "1.2.3"
  val parserVersion = "2.1.0"
  val scalaDomVersion = "2.0.0"
  val scalaLoggingVersion = "3.9.4"
  val scalatestVersion = "3.2.10"
  val sourceCodeVersion = "0.2.7"
  val sttpVersion = "3.3.18"
  val uPickleVersion = "1.4.3"

  // The following lines are all defined as inline settings to accommodate %%%,
  // which will not be resolved until used in the crossproject scope

  val domDeps = Def.setting(Seq(
    "org.scala-js" %%% "scalajs-dom" % scalaDomVersion
  ))
  val parsingDeps = Def.setting(Seq(
    "org.scala-lang.modules" %%% "scala-parser-combinators" % parserVersion
  ))
  val sharedUtilDeps = Def.setting(Seq(
    "org.typelevel" %%% "cats-core" % catsVersion,
    "com.lihaoyi" %%% "sourcecode" % sourceCodeVersion,
    "com.lihaoyi" %%% "upickle" % uPickleVersion
  ))
  val testDeps = Def.setting(Seq(
    "org.scalactic" %%% "scalactic" % scalatestVersion % "test",
    "org.scalatest" %%% "scalatest" % scalatestVersion % "test"
  ))
  val jvmLoggingDeps = Def.setting(Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion,
    "ch.qos.logback" % "logback-classic" % logbackVersion,
    "org.codehaus.janino" % "janino" % janinoVersion
  ))
  val httpDeps = Def.setting(Seq(
    "com.softwaremill.sttp.client3" %%% "core" % sttpVersion
  ))
  val jvmHttpDeps = Def.setting(Seq(
    "com.softwaremill.sttp.client3" %% "httpclient-backend" % sttpVersion
  ))
  val webserverDeps = Def.setting(Seq(
    "com.lihaoyi" %% "cask" % caskVersion
  ))
  val scalajs = Def.setting(Seq(
    "org.scala-lang.modules.scalajs" %%% "scalajs-jquery" % "0.6",
    "org.scala-lang.modules.scalajs" %% "scalajs-jasmine-test-framework" % scalaJSVersion % "test"
  ))
}