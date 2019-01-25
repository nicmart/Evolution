package evolution.app.codec.config

import evolution.app.codec.JsonCodec
import evolution.app.portfolio.dsl.{ Config, module, predefinedVars }
import evolution.geometry.Point
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.interpreter.{ EvolutionExpr, EvolutionSerializer }
import evolution.primitive.algebra.evolution.parser.EvolutionGrammar
import io.circe.Json
import evolution.data.EvaluationModule._
import evolution.primitive.FullModule
import evolution.primitive.algebra.evolution.Evolution.Expr

object DrawingJsonCodec extends JsonCodec[Config] {
  private val module = new FullModule[F]
  private val predefinedVars = List("left", "bottom", "right", "top")
  private val initialVarContext = new module.VarContext(predefinedVars)
  private val serializer = new EvolutionSerializer[F]
  private val evolutionExpr = new EvolutionExpr[F]
  private val grammar = EvolutionGrammar.parserGrammar(evolutionExpr)
  private val algebraParser = grammar.evolutionOfPoints
  private val stringParser = algebraParser.parser(predefinedVars)

  import module.ast.Type

  override def encode(t: Config): Json =
    Json.fromString(t.expr.run(serializer)(predefinedVars))

  override def decode(r: Json): Option[Config] =
    for {
      serialized <- r.asString
      _ = println(s"Parsing inside Json Codec: $serialized")
      parsed = module.parse(serialized, Type.Evo(Type.Point), evolutionExpr, initialVarContext)
      _ = parsed.left.map(println)
      expr <- parsed.toOption
    } yield Config(expr.asInstanceOf[Expr[F, F[Point]]])
}
