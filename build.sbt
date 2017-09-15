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
            "org.typelevel" %%% "cats" % "0.9.0",
            "com.chuusai" %%% "shapeless" % "2.3.2",
            "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
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
            "com.github.japgolly.scalajs-react" %%% "core" % "1.1.0",
            "com.github.japgolly.scalajs-react" %%% "extra" % "1.1.0",
            "com.lihaoyi" %%% "scalatags" % "0.6.2"
        ),

        jsDependencies ++= Seq(
            "org.webjars.bower" % "react" % "15.3.2" / "react-with-addons.js" minified "react-with-addons.min.js" commonJSName "React",
            "org.webjars.bower" % "react" % "15.3.2" / "react-dom.js" minified "react-dom.min.js" dependsOn "react-with-addons.js" commonJSName "ReactDOM",
            "org.webjars.bower" % "react" % "15.3.2" / "react-dom-server.js" minified  "react-dom-server.min.js" dependsOn "react-dom.js" commonJSName "ReactDOMServer"
        ),
        scalaJSUseMainModuleInitializer := true
    )

// Needed, so sbt finds the projects
lazy val coreJVM = core.jvm
lazy val coreJS = core.js
