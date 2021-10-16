resolvers += Resolver.bintrayRepo("vmunier", "scalajs")
resolvers += Resolver.sonatypeRepo("releases")

// note: sbt-web-scalajs 1.1.0 does not work with scala 1.3.0
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.7.1")
addSbtPlugin("com.vmunier" % "sbt-web-scalajs" % "1.0.9")
addSbtPlugin("ch.epfl.scala" % "sbt-web-scalajs-bundler" % "0.20.0")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.2.0")
