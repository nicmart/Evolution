import WebKeys._

lazy val commonSettings = List(
    organization := "nicmart",
    scalaVersion := "2.12.4",
    version      := "0.1.0-SNAPSHOT"
)

lazy val core = crossProject.
    crossType(CrossType.Pure).
    in(file("core")).
    settings(
        inThisBuild(commonSettings),
        name := "test", // default name would be p1
        libraryDependencies ++= Seq(
            "org.scalatest" %%% "scalatest" % "3.0.4" % Test,
            "org.typelevel" %%% "cats" % "0.9.0",
            "com.chuusai" %%% "shapeless" % "2.3.2",
            "org.scalacheck" %%% "scalacheck" % "1.13.4" % Test,
            "com.lihaoyi" %%% "fastparse" % "1.0.0"
        )
    ).
    jvmSettings(
    ).
    jsSettings(
    )

lazy val jsApp = project.
    in(file("app"))
    .dependsOn(core.js % "test->test;compile->compile")
    .enablePlugins(ScalaJSPlugin, ScalaJSWeb)
    .settings(
        inThisBuild(commonSettings),
        name := "jsApp",
        version      := "0.1.0-SNAPSHOT",
        libraryDependencies ++= Seq(
            "org.scala-js" %%% "scalajs-dom" % "0.9.2",
            "com.github.japgolly.scalajs-react" %%% "core" % "1.1.1",
            "com.github.japgolly.scalajs-react" %%% "extra" % "1.1.1",
            "com.github.japgolly.scalajs-react" %%% "test" % "1.1.1" % "test",
            "com.lihaoyi" %%% "scalatags" % "0.6.2",
            "io.circe" %%% "circe-core" % "0.8.0",
            "io.circe" %%% "circe-generic" % "0.8.0",
            "io.circe" %%% "circe-parser" % "0.8.0"
        ),
        jsDependencies ++= Seq(
            "org.webjars.bower" % "react" % "15.6.1" / "react-with-addons.js" minified "react-with-addons.min.js" commonJSName "React",
            "org.webjars.bower" % "react" % "15.6.1" / "react-dom.js" minified "react-dom.min.js" dependsOn "react-with-addons.js" commonJSName "ReactDOM",
            "org.webjars.bower" % "react" % "15.6.1" / "react-dom-server.js" minified  "react-dom-server.min.js" dependsOn "react-dom.js" commonJSName "ReactDOMServer"
        ),
        scalaJSUseMainModuleInitializer := true,
        scalaJSStage in Global := FastOptStage,
        // jsEnv in Test := new PhantomJS2Env(scalaJSPhantomJSClassLoader.value),
        jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv

    )

lazy val server = (project in file("server")).settings(
  scalaJSProjects := Seq(jsApp),
  pipelineStages in Assets := Seq(scalaJSPipeline),
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-http" % "10.0.11"
  ),
  (managedClasspath in Runtime) += (packageBin in Assets).value,
  packagePrefix in Assets := "public/",
  compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline.map(f => f(Seq.empty))).value
).enablePlugins(SbtWeb)

// Needed, so sbt finds the projects
lazy val coreJVM = core.jvm
lazy val coreJS = core.js
