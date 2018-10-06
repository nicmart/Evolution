import WebKeys._

autoCompilerPlugins := true
resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.8" cross CrossVersion.binary)


lazy val commonSettings = List(
  organization := "nicmart",
  scalaVersion := "2.12.6",
  version      := "0.1.0-SNAPSHOT",
  scalacOptions += "-Ypartial-unification",
  libraryDependencies ++= Seq(
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8" cross CrossVersion.binary)
  )
)

lazy val core = crossProject.
    crossType(CrossType.Full).
    //in(file("core")).
    settings(
        name := "core",
        inThisBuild(commonSettings),
        libraryDependencies ++= Seq(
            "org.scalatest" %%% "scalatest" % "3.0.4" % Test,
            "org.typelevel" %%% "cats-core" % "1.3.1",
            "com.chuusai" %%% "shapeless" % "2.3.3",
            "org.scalacheck" %%% "scalacheck" % "1.13.4" % Test,
            "com.lihaoyi" %%% "fastparse" % "1.0.0",
            compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4" cross CrossVersion.binary)
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
        version      := "0.1.0-SNAPSHOT",
        libraryDependencies ++= Seq(
            "org.scala-js" %%% "scalajs-dom" % "0.9.2",
            "com.github.japgolly.scalajs-react" %%% "core" % "1.1.1",
            "com.github.japgolly.scalajs-react" %%% "extra" % "1.1.1",
            "com.github.japgolly.scalajs-react" %%% "test" % "1.1.1" % "test",
            "com.lihaoyi" %%% "scalatags" % "0.6.2",
            "io.circe" %%% "circe-core" % "0.9.3",
            "io.circe" %%% "circe-generic" % "0.9.3",
            "io.circe" %%% "circe-parser" % "0.9.3"
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

lazy val theory = (project in file("theory"))
  .settings(
    inThisBuild(commonSettings),
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.0.4" % Test,
      "com.lihaoyi" %%% "fastparse" % "1.0.0",
      "com.chuusai" %%% "shapeless" % "2.3.3"
    )
  )

// Needed, so sbt finds the projects
lazy val coreJVM = core.jvm
lazy val coreJS = core.js