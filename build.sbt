import Dependencies._

lazy val commonSettings = List(
    organization := "nicmart",
    scalaVersion := "2.12.1",
    version      := "0.1.0-SNAPSHOT"
)

lazy val core = crossProject.
    crossType(CrossType.Pure).
    in(file("core")).
    settings(
        inThisBuild(commonSettings),
        name := "test", // default name would be p1
        libraryDependencies ++= Seq(
            scalaTest % Test,
            "org.typelevel" %% "cats" % "0.9.0"
        )
    ).
    jvmSettings(
    ).
    jsSettings(
    )

lazy val jsApp = project.
    in(file("app"))
    .dependsOn(core.js)
    .enablePlugins(ScalaJSPlugin, WorkbenchPlugin)
    .settings(
        inThisBuild(commonSettings),
        name := "jsApp",
        version      := "0.1.0-SNAPSHOT",
        libraryDependencies ++= Seq(
            "org.scala-js" %%% "scalajs-dom" % "0.9.1",
            "com.lihaoyi" %%% "scalatags" % "0.6.2"
        )
    )

// Needed, so sbt finds the projects
lazy val coreJVM = core.jvm
lazy val coreJS = core.js

//lazy val p2 = crossProject.crossType(CrossType.Pure).dependsOn(p1 % "test")
//
//// Needed, so sbt finds the projects
//lazy val p2JVM = p2.jvm
//lazy val p2JS = p2.js
