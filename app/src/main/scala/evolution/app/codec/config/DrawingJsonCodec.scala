package evolution.app.codec.config

import evolution.app.codec.JsonCodec
import evolution.app.portfolio.dsl.Config
import evolution.geometry.Point
import evolution.primitive.algebra
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.interpreter.Serializer
import evolution.primitive.algebra.parser.DrawingAlgebraParser
import io.circe.Json

object DrawingJsonCodec extends JsonCodec[Config] {
  override def encode(t: Config): Json =
    Json.fromString(t.run(Serializer)(Nil))
  // TODO this is rubbish
  override def decode(r: Json): Option[Config] = {
    val algabraParser = new DrawingAlgebraParser(Serializer)
    val container = algabraParser.container
    val stringParser = algabraParser.container.dependentPointParserF.parser(container)
    for {
      serialized <- r.asString
      _ = println(serialized)
      drawing <- {
        val isValid = stringParser.parse(serialized).fold[Boolean]((_, _, _) => false, (_, _) => true)
        if (!isValid) {
          println("Invalid expression")
          None
        } else {
          println("Successful expression")
          Some(new Config {
            override def run[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String]): R[F[Point]] = {
              val algebraParser = new DrawingAlgebraParser(alg)
              val parser = algebraParser.container.pointParserF
              parser.parse(serialized).get.value
            }
          })
        }
      }
    } yield drawing
  }
}
