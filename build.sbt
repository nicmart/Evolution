import WebKeys._
import sbt.Keys.resolvers

Test / fork := true

// Experimental, turn off in case of problems
//ThisBuild / turbo := true
ThisBuild / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.ScalaLibrary
Global / onChangedBuildSource := ReloadOnSourceChanges

logBuffered in Test := false
bloopExportJarClassifiers in Global := Some(Set("sources"))

lazy val options = Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8", // Specify character encoding used by source files.
  "-explaintypes", // Explain type errors in more detail.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
  "-Xfatal-warnings", // Fail the compilation if there are any warnings.
  "-Xfuture", // Turn on future language features.
  "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
  "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
  "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
  "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
  "-Xlint:option-implicit", // Option.apply used implicit view.
  "-Xlint:package-object-classes", // Class or object defined in package object.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match", // Pattern match may not be typesafe.
  "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ywarn-dead-code", // Warn when dead code is identified.
  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen", // Warn when numerics are widened.
  "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals", // Warn if a local definition is unused.
  "-Ywarn-unused:params", // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates", // Warn if a private member is unused.
  "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
)

lazy val commonSettings = List(
  organization := "nicmart",
  scalaVersion := "3.1.0",
  version := "0.1.0-SNAPSHOT",
//  scalacOptions ++= Seq("-deprecation", "-rewrite", "-indent", "-source 3.0-migration"),
  scalacOptions ++= Seq("-deprecation"),
  autoCompilerPlugins := true,
  resolvers += Resolver.sonatypeRepo("releases")
)

lazy val core = project
  .in(file("core"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    inThisBuild(commonSettings),
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.2.10" % "test",
      "org.scalatestplus" %%% "scalacheck-1-15" % "3.2.10.0" % "test",
      "org.scalacheck" %%% "scalacheck" % "1.15.4",
      "org.typelevel" %%% "cats-core" % "2.6.1",
      "com.lihaoyi" %%% "pprint" % "0.6.6",
      "org.typelevel" %%% "cats-parse" % "0.3.4"
    )
  )

lazy val jsAppSettings =
  Seq(
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.0.0",
      "com.github.japgolly.scalajs-react" %%% "core" % "2.0.0-RC4",
      "com.github.japgolly.scalajs-react" %%% "extra" % "2.0.0-RC4",
      "io.circe" %%% "circe-core" % "0.14.1",
      "io.circe" %%% "circe-generic" % "0.14.1",
      "io.circe" %%% "circe-parser" % "0.14.1",
      "io.github.cquiroz" %%% "scala-java-time" % "2.3.0"
    )
  )

// Creating this "clone" project was the only way I found to be able to run
// jsAppTest/test without the "ReferenceError" for core module exported classes
lazy val jsAppTest = project
  .in(file("app"))
  .dependsOn(core % "test->test;compile->compile")
  .enablePlugins(ScalaJSPlugin)
  .settings(
    inThisBuild(commonSettings),
    jsAppSettings,
    target := {
      (ThisBuild / baseDirectory).value / "target" / thisProject.value.id
    }
  )

lazy val jsApp = project
  .in(file("app"))
  .dependsOn(core % "test->test;compile->compile")
  .enablePlugins(ScalaJSBundlerPlugin)
  .settings(
    inThisBuild(commonSettings),
    jsAppSettings,
    npmDependencies in Compile ++= Seq(
      "react" -> "16.7.0",
      "react-dom" -> "16.7.0",
      "codemirror" -> "5.48.4",
      "react-codemirror2" -> "6.0.0"
    ),
    webpackBundlingMode := BundlingMode.LibraryAndApplication(),
    scalaJSUseMainModuleInitializer := true,
    scalaJSStage in Global := FastOptStage
  )

lazy val server = (project in file("server"))
  .settings(
    scalaJSProjects := Seq(jsApp),
    pipelineStages in Assets := Seq(scalaJSPipeline),
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-dsl" % "1.0.0-M29",
      "org.http4s" %% "http4s-blaze-server" % "1.0.0-M29",
      "ch.qos.logback" % "logback-classic" % "1.2.3"
    ),
    (managedClasspath in Runtime) += (packageBin in Assets).value,
    packagePrefix in Assets := "public/",
    npmAssets ++= NpmAssets
      .ofProject(jsApp) { nodeModules =>
        (nodeModules / "codemirror").allPaths // sbt 1.0.0+
      }
      .value
  )
  .enablePlugins(SbtWeb, WebScalaJSBundlerPlugin)

addCommandAlias("testAll", "; core/test; jsAppTest/test")
addCommandAlias("compileAll", "; compile; test:compile")
