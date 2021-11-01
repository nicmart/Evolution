import WebKeys._
import sbt.Keys.resolvers

Test / fork := true

// Experimental, turn off in case of problems
//ThisBuild / turbo := true
ThisBuild / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.ScalaLibrary
Global / onChangedBuildSource := ReloadOnSourceChanges

Test / logBuffered := false
Global / bloopExportJarClassifiers := Some(Set("sources"))

lazy val commonSettings = List(
  organization := "nicmart",
  scalaVersion := "3.1.0",
  version := "0.1.0-SNAPSHOT",
//  scalacOptions ++= Seq("-deprecation", "-rewrite", "-indent", "-source 3.0-migration"),
  scalacOptions ++= Seq("-deprecation", "-indent", "-Ykind-projector:underscores"),
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

lazy val jsApp = project
  .in(file("app"))
  .dependsOn(core % "test->test;compile->compile")
  .enablePlugins(ScalaJSBundlerPlugin)
  .settings(
    inThisBuild(commonSettings),
    jsAppSettings,
    Compile / npmDependencies ++= Seq(
      "react" -> "16.7.0",
      "react-dom" -> "16.7.0",
      "codemirror" -> "5.48.4",
      "react-codemirror2" -> "6.0.0"
    ),
    webpackBundlingMode := BundlingMode.LibraryAndApplication(),
    // source map generation was failing after scala3 migration
    // see also https://github.com/scalacenter/scalajs-bundler/issues/385
    webpackEmitSourceMaps := false,
    scalaJSUseMainModuleInitializer := true,
    Global / scalaJSStage := FullOptStage,
    Test / scalaJSStage := FastOptStage
  )

lazy val server = (project in file("server"))
  .settings(
    scalaJSProjects := Seq(jsApp),
    Assets / pipelineStages := Seq(scalaJSPipeline),
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-dsl" % "1.0.0-M29",
      "org.http4s" %% "http4s-blaze-server" % "1.0.0-M29",
      "ch.qos.logback" % "logback-classic" % "1.2.3"
    ),
    (Runtime / managedClasspath) += (packageBin in Assets).value,
    Assets / packagePrefix := "public/",
    npmAssets ++= NpmAssets
      .ofProject(jsApp) { nodeModules =>
        (nodeModules / "codemirror").allPaths // sbt 1.0.0+
      }
      .value
  )
  .enablePlugins(SbtWeb, WebScalaJSBundlerPlugin)

addCommandAlias("testAll", "; core/test; jsAppTest/test")
addCommandAlias("compileAll", "; compile; test:compile")
