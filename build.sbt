
name := "grogr"
val commonSettings = Seq(
  scalaVersion := "3.1.0"
)

lazy val root = (project in file("."))
  .settings(commonSettings)
  .aggregate(app, core.js, core.jvm, sql)

lazy val core = crossProject(JSPlatform, JVMPlatform).in(file("core"))
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.0",
      "com.lihaoyi" %%% "sourcecode" % "0.2.7",
      "org.scalactic" %%% "scalactic" % "3.2.10",
      "org.scalatest" %%% "scalatest" % "3.2.10" % "test"
    )
  ).jvmSettings(
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "org.codehaus.janino" % "janino" % "3.1.6"
    ),
    Test / fork := true
  )

lazy val app = (project in file("app"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    commonSettings,
    // This is an application with a main method
    scalaJSUseMainModuleInitializer := true,
    mainClass := Some("tutorial.webapp.TutorialApp"),
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.0.0"
  )

lazy val sql = (project in file("sql"))