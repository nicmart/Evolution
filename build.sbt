import WebKeys._
import sbt.Keys.resolvers
import sbtcrossproject.CrossPlugin.autoImport.{ crossProject, CrossType }

Test / fork := true

// Experimental, turn off in case of problems
//ThisBuild / turbo := true
ThisBuild / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.ScalaLibrary

bloopExportJarClassifiers in Global := Some(Set("sources"))
logBuffered in Test := false

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

lazy val jvmScalatestSettings = Test / testOptions ++= Seq(
  Tests.Argument(TestFrameworks.ScalaTest, "-oSD"),
  Tests.Argument(TestFrameworks.ScalaTest, "-W", "1", "1")
)

lazy val commonSettings = List(
  organization := "nicmart",
  scalaVersion := "2.13.0",
  version := "0.1.0-SNAPSHOT",
  scalacOptions ++= Seq(
    "-Xlint:unused",
    "-Xlint:inaccessible",
    "-Wdead-code"
  ),
  autoCompilerPlugins := true,
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.10.3" cross CrossVersion.binary),
  libraryDependencies ++= Seq(
    compilerPlugin("com.github.ghik" %%% "silencer-plugin" % "1.4.2"),
    "com.github.ghik" %%% "silencer-lib" % "1.4.2" % Provided
  )
)

// FROM https://github.com/portable-scala/sbt-crossproject/blob/ec514cb96892cb27e3376a5c5d9914b5ae86c69b/sbt-crossproject/src/main/scala/sbtcrossproject/CrossProject.scala#L207
// Since CrossBuild by default do things only for Test Config
def makeCrossSources(sharedSrcDir: Option[File], scalaBinaryVersion: String): Seq[File] = {
  sharedSrcDir match {
    case Some(dir) =>
      Seq(dir.getParentFile / s"${dir.name}-$scalaBinaryVersion", dir)
    case None => Seq()
  }
}

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .settings(
    name := "core",
    inThisBuild(commonSettings),
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.0.8" % "test",
      "org.typelevel" %%% "cats-core" % "2.0.0-RC2",
      "org.scalacheck" %%% "scalacheck" % "1.14.0",
      "com.lihaoyi" %%% "fastparse" % "2.1.3",
      "com.lihaoyi" %%% "pprint" % "0.5.5",
      "com.beachape" %%% "enumeratum" % "1.5.13"
    )
  )
  .jvmSettings(
    jvmScalatestSettings
  )

lazy val jsApp = project
  .in(file("app"))
  .dependsOn(core.js % "test->test;compile->compile")
  .enablePlugins(ScalaJSPlugin, ScalaJSWeb, ScalaJSBundlerPlugin)
  .settings(
    inThisBuild(commonSettings),
    version := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.9.7",
      "com.github.japgolly.scalajs-react" %%% "core" % "1.5.0-RC2",
      "com.github.japgolly.scalajs-react" %%% "extra" % "1.5.0-RC2",
      "com.lihaoyi" %%% "scalatags" % "0.7.0",
      "io.circe" %%% "circe-core" % "0.12.0-RC4",
      "io.circe" %%% "circe-generic" % "0.12.0-RC4",
      "io.circe" %%% "circe-parser" % "0.12.0-RC4",
      "io.github.cquiroz" %%% "scala-java-time" % "2.0.0-RC3"
    ),
    npmDependencies in Compile ++= Seq(
      "react" -> "16.7.0",
      "react-dom" -> "16.7.0",
      "codemirror" -> "5.48.4",
      "react-codemirror2" -> "6.0.0"
    ),
    scalaJSUseMainModuleInitializer := true,
    scalaJSStage in Global := FastOptStage
  )

lazy val server = (project in file("server"))
  .settings(
    scalaJSProjects := Seq(jsApp),
    pipelineStages in Assets := Seq(scalaJSPipeline),
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-dsl" % "0.21.0-M4",
      "org.http4s" %% "http4s-blaze-server" % "0.21.0-M4",
      "ch.qos.logback" % "logback-classic" % "1.2.3"
    ),
    (managedClasspath in Runtime) += (packageBin in Assets).value,
    packagePrefix in Assets := "public/",
    compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline.map(f => f(Seq.empty))).value,
    npmAssets ++= NpmAssets
      .ofProject(jsApp) { nodeModules =>
        (nodeModules / "codemirror").allPaths // sbt 1.0.0+
      }
      .value
  )
  .enablePlugins(SbtWeb, WebScalaJSBundlerPlugin)

// Needed, so sbt finds the projects
lazy val coreJVM = core.jvm
lazy val coreJS = core.js

addCommandAlias("testAll", "; coreJVM/test; jsApp/test")
addCommandAlias("compileAll", "; compile; test/compile")
