package evolution.app.codec.config

import evolution.app.codec.JsonCodec
import evolution.app.portfolio.dsl.Config
import evolution.geometry.Point
import evolution.primitive.algebra.ConstString
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.interpreter.EvolutionSerializer
import evolution.primitive.algebra.evolution.parser.EvolutionGrammar
import io.circe.Json

object DrawingJsonCodec extends JsonCodec[Config] {
  val serializer = new EvolutionSerializer[ConstString, ConstString]
  override def encode(t: Config): Json =
    Json.fromString(t.run(serializer)(Nil))
  // TODO this is rubbish
  // TODO this is awful
  override def decode(r: Json): Option[Config] = {
    val grammar = EvolutionGrammar.grammar(serializer)
    val algebraParser = grammar.chain.evolutionOf[Point](grammar.constants.points)
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
            override def run[S[_], F[_], R[_]](alg: Evolution[S, F, R, Double, String, String]): R[F[Point]] = {
              val grammar = EvolutionGrammar.grammar(alg)
              val algebraParser = grammar.chain.evolutionOf[Point](grammar.constants.points)
              val parser = algebraParser(Nil)
              println("Parse inside config")

              val r = parser.parse(serialized).get.value
              println(s"This was parsed: $r")
              r
            }
          })
        }
      }
    } yield drawing
  }
}
