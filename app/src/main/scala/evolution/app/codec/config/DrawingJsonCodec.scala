package evolution.app.codec.config

import evolution.app.codec.JsonCodec
import evolution.app.portfolio.dsl.Config
import evolution.geometry.Point
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.interpreter.EvolutionSerializer
import evolution.primitive.algebra.evolution.parser.EvolutionGrammar
import io.circe.Json

object DrawingJsonCodec extends JsonCodec[Config] {
  override def encode(t: Config): Json =
    Json.fromString(t.run(EvolutionSerializer)(Nil))
  // TODO this is rubbish
  // TODO this is awful
  override def decode(r: Json): Option[Config] = {
    val grammar = EvolutionGrammar.grammar(EvolutionSerializer)
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
            override def run[S[_], F[_], R[_]](alg: Evolution[S, F, R, String]): R[F[Point]] = {
              val grammar = EvolutionGrammar.grammar(alg)
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
