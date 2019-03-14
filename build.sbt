import WebKeys._
import sbt.Keys.resolvers
import sbtcrossproject.CrossPlugin.autoImport.{ crossProject, CrossType }

Test / fork := true

lazy val options = Seq(
  "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
  "-encoding", "utf-8",                // Specify character encoding used by source files.
  "-explaintypes",                     // Explain type errors in more detail.
  "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
  "-language:higherKinds",             // Allow higher-kinded types
  "-language:implicitConversions",     // Allow definition of implicit functions called views
  "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
  "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
  "-Xfuture",                          // Turn on future language features.
  "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
  "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
  "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
  "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
  "-Xlint:option-implicit",            // Option.apply used implicit view.
  "-Xlint:package-object-classes",     // Class or object defined in package object.
  "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match",              // Pattern match may not be typesafe.
  "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ypartial-unification",             // Enable partial unification in type constructor inference
  "-Ywarn-dead-code",                  // Warn when dead code is identified.
  "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
  "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen",              // Warn when numerics are widened.
  "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",              // Warn if a local definition is unused.
  "-Ywarn-unused:params",              // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",            // Warn if a private member is unused.
  "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
)

lazy val commonSettings = List(
  organization := "nicmart",
  scalaVersion := "2.12.8", // Can't upgrade to 2.12.7 until https://github.com/scala/bug/issues/11174 is fixed
  version := "0.1.0-SNAPSHOT",
  scalacOptions ++= Seq(
    "-Ypartial-unification",
    "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",              // Warn if a local definition is unused.
    "-Ywarn-unused:params",              // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates",
    "-Ywarn-inaccessible",
    "-Ywarn-dead-code"
  ),
    Test / testOptions ++= List(
    Tests.Argument(TestFrameworks.ScalaTest, "-oSD"),
    Tests.Argument(TestFrameworks.ScalaTest, "-W", "1", "1")
  ),
  autoCompilerPlugins := true,
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.8" cross CrossVersion.binary),
  libraryDependencies ++= Seq(
    compilerPlugin("com.github.ghik" %%% "silencer-plugin" % "1.3.1"),
    "com.github.ghik" %%% "silencer-lib" % "1.3.1" % Provided
  )
)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .settings(
    name := "core",
    inThisBuild(commonSettings),
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oSD"),
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.0.4" % Test,
      "org.typelevel" %%% "cats-core" % "1.3.1",
      "org.typelevel" %%% "cats-mtl-core" % "0.4.0",
      "com.chuusai" %%% "shapeless" % "2.3.3",
      "org.scalacheck" %%% "scalacheck" % "1.13.4",
      "com.lihaoyi" %%% "fastparse" % "1.0.0",
      "com.beachape" %%% "enumeratum" % "1.5.13",
      "com.propensive" %%% "contextual" % "1.1.0"

    )
  )
  .jsSettings(
    )
  .jsSettings(
    )

lazy val jsApp = project
  .in(file("app"))
  .dependsOn(core.js % "test->test;compile->compile")
  .enablePlugins(ScalaJSPlugin, ScalaJSWeb)
  .settings(
    inThisBuild(commonSettings),
    version := "0.1.0-SNAPSHOT",
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
      "org.webjars.bower" % "react" % "15.6.1" / "react-dom-server.js" minified "react-dom-server.min.js" dependsOn "react-dom.js" commonJSName "ReactDOMServer"
    ),
    scalaJSUseMainModuleInitializer := true,
    scalaJSStage in Global := FastOptStage,
    // jsEnv in Test := new PhantomJS2Env(scalaJSPhantomJSClassLoader.value),
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
  )

lazy val server = (project in file("server"))
  .settings(
    scalaJSProjects := Seq(jsApp),
    pipelineStages in Assets := Seq(scalaJSPipeline),
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http" % "10.0.11"
    ),
    (managedClasspath in Runtime) += (packageBin in Assets).value,
    packagePrefix in Assets := "public/",
    compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline.map(f => f(Seq.empty))).value
  )
  .enablePlugins(SbtWeb)

// Needed, so sbt finds the projects
lazy val coreJVM = core.jvm
lazy val coreJS = core.js
