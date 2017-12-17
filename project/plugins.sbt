resolvers += Resolver.bintrayRepo("vmunier", "scalajs")
resolvers += Resolver.sonatypeRepo("releases")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.21")
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.9.0")
addSbtPlugin("com.vmunier" % "sbt-web-scalajs" % "1.0.6")
addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")
