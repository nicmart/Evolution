resolvers += Resolver.bintrayRepo("vmunier", "scalajs")
resolvers += Resolver.sonatypeRepo("releases")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.32")

// Use Scalajs 0.6 specific version
addSbtPlugin("com.vmunier" % "sbt-web-scalajs" % "1.0.9-0.6")
addSbtPlugin("ch.epfl.scala" % "sbt-web-scalajs-bundler" % "0.15.0-0.6")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.3.2")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.2.0")
//addCompilerPlugin("ch.epfl.scala" %% "scalac-profiling" % "1.0.0")
