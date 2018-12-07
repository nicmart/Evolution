package evolution.app.codec.config

import evolution.app.codec.JsonCodec
import evolution.app.portfolio.dsl.Config
import evolution.geometry.Point
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.interpreter.{ EvolutionExpr, EvolutionSerializer }
import evolution.primitive.algebra.evolution.parser.EvolutionGrammar
import io.circe.Json
import evolution.data.EvaluationModule._

object DrawingJsonCodec extends JsonCodec[Config] {
  private val serializer = new EvolutionSerializer[F]
  private val evolutionExpr = new EvolutionExpr[F]
  private val grammar = EvolutionGrammar.parserGrammar(evolutionExpr)
  private val algebraParser = grammar.evolutionOfPoints
  private val stringParser = algebraParser.parser(Nil)

  override def encode(t: Config): Json =
    Json.fromString(t.expr.run(serializer)(Nil))

  override def decode(r: Json): Option[Config] =
    for {
      serialized <- r.asString
      _ = println("Parsing inside Json Codec")
      expr <- stringParser
        .parse(serialized)
        .fold[Option[Evolution.Expr[F, F[Point]]]]((_, _, _) => None, (expr, _) => Some(expr))
    } yield Config(expr)
}
