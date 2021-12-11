
name := "grogr"
scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .aggregate(app, core, sql)

lazy val core = (project in file("core"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.0",
      "org.scalactic" %%% "scalactic" % "3.2.10",
      "org.scalatest" %%% "scalatest" % "3.2.10" % "test"
    )
  )

lazy val app = (project in file("app"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    // This is an application with a main method
    scalaJSUseMainModuleInitializer := true,
    mainClass := Some("tutorial.webapp.TutorialApp"),
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"
  )

lazy val sql = (project in file("sql"))
  .enablePlugins(ScalaJSPlugin)