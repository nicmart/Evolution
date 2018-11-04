package evolution.app.codec.config

import cats.Id
import evolution.algebra.representation.RNGRepr
import evolution.app.codec.JsonCodec
import evolution.app.portfolio.dsl.Config
import evolution.geometry.Point
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.interpreter.{EvolutionExpr, EvolutionSerializer}
import evolution.primitive.algebra.evolution.parser.{EvolutionGrammar}
import io.circe.Json

object DrawingJsonCodec extends JsonCodec[Config] {
  private val serializer = new EvolutionSerializer[RNGRepr]
  private val evolutionExpr = new EvolutionExpr[RNGRepr]
  private val grammar = EvolutionGrammar.grammar(evolutionExpr)
  private val algebraParser = grammar.chain.evolutionOf[Point](grammar.constants.points)
  private val stringParser = algebraParser.parser(Nil)

  override def encode(t: Config): Json =
    Json.fromString(t.expr.run(serializer)(Nil))

  override def decode(r: Json): Option[Config] =
    for {
      serialized <- r.asString
      _ = println("Parsing inside Json Codec")
      expr <- stringParser
        .parse(serialized)
        .fold[Option[Evolution.Expr[RNGRepr, RNGRepr[Point]]]]((_, _, _) => None, (expr, _) => Some(expr))
    } yield Config(expr)
}
