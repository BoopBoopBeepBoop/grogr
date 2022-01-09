import Dependencies._
import sbt.Test

/*
 ╔════════════════════╗
 ║   Common Settings  ║
 ╚════════════════════╝
*/

name := "grogr"

val commonSettings = Seq(
  scalaVersion := "3.1.0"
)

val jvmCommonSettings = Seq(
  Test / fork := true
)

/*
 ╔════════════════════╗
 ║ Project Definition ║
 ╚════════════════════╝
*/

lazy val root = (project in file("."))
  .settings(commonSettings)
  .aggregate(app, core.js, core.jvm, engine.js, engine.jvm, server, driver)

lazy val core = crossProject(JSPlatform, JVMPlatform).in(file("core"))
  .settings(
    commonSettings,
    libraryDependencies ++= parseAndSqlDeps.value ++ sharedUtilDeps.value ++ testDeps.value
  ).jvmSettings(
    jvmCommonSettings,
    libraryDependencies ++= jvmLoggingDeps.value
  )

lazy val driver = (project in file("driver"))
  .settings(commonSettings)
  .aggregate(clickhouseDriver, grogrDriver.js, grogrDriver.jvm)

lazy val clickhouseDriver = (project in (file("driver") / "clickhouse"))
  .settings(
    commonSettings,
    jvmCommonSettings,
    libraryDependencies ++= httpDeps.value ++ jvmHttpDeps.value ++ sharedUtilDeps.value ++ testDeps.value
  ).dependsOn(core.jvm)

lazy val grogrDriver = crossProject(JSPlatform, JVMPlatform).in(file("driver") / "grogr")
  .settings(
    commonSettings,
    libraryDependencies ++= httpDeps.value ++ testDeps.value
  )
  .jvmSettings(
    jvmCommonSettings,
    libraryDependencies ++= jvmHttpDeps.value
  )
  .dependsOn(core)

lazy val engine = crossProject(JSPlatform, JVMPlatform).in(file("engine"))
  .settings(
    commonSettings,
    libraryDependencies ++= httpDeps.value ++ testDeps.value
  )
  .jvmSettings(
    jvmCommonSettings,
    libraryDependencies ++= jvmHttpDeps.value
  )
  .dependsOn(core)

lazy val app = (project in file("app"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    commonSettings,
    // This is an application with a main method
    scalaJSUseMainModuleInitializer := true,
    mainClass := Some("tutorial.webapp.TutorialApp"),
    libraryDependencies ++= domDeps.value
  )

lazy val server = (project in file("server"))
  .enablePlugins(JavaServerAppPackaging, DockerPlugin, DockerComposePlugin)
  .settings(
    commonSettings,
    jvmCommonSettings,
    mainClass := Some("grogr.server.Application"),
    run / fork := true,
    Test / fork := true,
    libraryDependencies ++= webserverDeps.value,

    dockerBaseImage := "openjdk:17",
    dockerUpdateLatest := true,
    Docker / packageName := "grogr/server"
  )
  .dependsOn(core.jvm, engine.jvm, clickhouseDriver)