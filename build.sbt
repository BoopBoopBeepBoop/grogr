import Dependencies._

/*
 ╔════════════════════╗
 ║   Common Settings  ║
 ╚════════════════════╝
*/

name := "grogr"

val commonSettings = Seq(
  scalaVersion := "3.1.0",
  Test / fork := true
)

/*
 ╔════════════════════╗
 ║ Project Definition ║
 ╚════════════════════╝
*/

lazy val root = (project in file("."))
  .settings(commonSettings)
  .aggregate(app, core.js, core.jvm, engine.js, engine.jvm, server)

lazy val core = crossProject(JSPlatform, JVMPlatform).in(file("core"))
  .settings(
    commonSettings,
    libraryDependencies ++= parsingDeps.value ++ sharedUtilDeps.value ++ testDeps.value
  ).jvmSettings(
    libraryDependencies ++= jvmLoggingDeps.value
  )

lazy val engine = crossProject(JSPlatform, JVMPlatform).in(file("engine"))
  .settings(
    commonSettings,
    libraryDependencies ++= httpDeps.value ++ testDeps.value
  )
  .jvmSettings(
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
    mainClass := Some("grogr.server.Application"),
    run / fork := true,
    libraryDependencies ++= webserverDeps.value,

    dockerBaseImage := "openjdk:17",
    dockerUpdateLatest := true,
    Docker / packageName := "grogr/server"
  )