import Dependencies._

lazy val commonSettings = List(
    organization := "nicmart",
    scalaVersion := "2.12.1",
    version      := "0.1.0-SNAPSHOT"
)

lazy val core = (project in file("core")).
  settings(
    inThisBuild(commonSettings),
    name := "Evolution",
    libraryDependencies ++= Seq(
        scalaTest % Test,
        "org.typelevel" %% "cats" % "0.9.0"
    )
  )
