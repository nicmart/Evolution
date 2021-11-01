resolvers += Resolver.bintrayRepo("vmunier", "scalajs")
resolvers += Resolver.sonatypeRepo("releases")

addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.4.9")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.7.1")
addSbtPlugin("com.vmunier" % "sbt-web-scalajs" % "1.2.0")
addSbtPlugin("ch.epfl.scala" % "sbt-web-scalajs-bundler" % "0.20.0")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.3")
