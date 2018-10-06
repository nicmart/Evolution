package evolution.app.codec.config

import evolution.app.codec.JsonCodec
import evolution.app.portfolio.dsl.Config
import evolution.geometry.Point
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.evolution.interpreter.EvolutionAlgebraSerializer
import evolution.primitive.algebra.evolution.parser.EvolutionAlgebraGrammar
import io.circe.Json

object DrawingJsonCodec extends JsonCodec[Config] {
  override def encode(t: Config): Json =
    Json.fromString(t.run(EvolutionAlgebraSerializer)(Nil))
  // TODO this is rubbish
  // TODO this is awful
  override def decode(r: Json): Option[Config] = {
    val grammar = EvolutionAlgebraGrammar.grammar(EvolutionAlgebraSerializer)
    val algebraParser = grammar.list.evolutionOf[Point](grammar.constants.points)
    val stringParser = algebraParser(Nil)
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
              val grammar = EvolutionAlgebraGrammar.grammar(alg)
              val algebraParser = grammar.list.evolutionOf[Point](grammar.constants.points)
              val parser = algebraParser(Nil)
              parser.parse(serialized).get.value
            }
          })
        }
      }
    } yield drawing
  }
}
