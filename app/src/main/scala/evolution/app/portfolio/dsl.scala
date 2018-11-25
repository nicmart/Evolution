package evolution.app.portfolio

import cats.Id
import evolution.algebra.representation.RNGRepr
import evolution.app.codec.JsonCodec
import evolution.app.codec.config.DrawingJsonCodec
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.model.state.DrawingState
import evolution.app.react.component.config.ConfigComponent.instance
import evolution.app.react.component.config.{ ConfigComponent, instances }
import evolution.geometry.Point
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.interpreter.{ EvolutionEvaluator, EvolutionExpr, EvolutionSerializer }
import evolution.primitive.algebra.evolution.parser.EvolutionGrammar
import evolution.random.RNG
import japgolly.scalajs.react.vdom.html_<^._

object dsl extends DrawingDefinition[Point] {
  val name = "drawing dsl"

  type Expr[T] = Evolution.Expr[RNGRepr, RNGRepr[Point]]

  private val serializer = new EvolutionSerializer[RNGRepr]
  private val evolutionExpr = new EvolutionExpr[RNGRepr]
  private val grammar = EvolutionGrammar.parserGrammar(evolutionExpr)
  private val algebraParser = grammar.evolutionOfPoints
  private val stringParser = algebraParser.parser(Nil)

  case class Config(expr: Expr[Point])

  override val configComponent: ConfigComponent[Config] = {
    instance[Config]("drawing config") { (config2Snapshot, children) =>
      val stringSnapshot =
        config2Snapshot.zoomState[String](config2 => config2.expr.run(serializer)(Nil)) {
          serialized => previousConfig =>
            {
              println("parsing inside configComponent")
              stringParser
                .parse(serialized)
                .fold((_, _, failure) => { println(failure); previousConfig }, (drawing, _) => Config(drawing))
            }
        }
      val component: ConfigComponent[String] = instances.textConfig
      component.apply(stringSnapshot)()
    }
  }

  override def stream(ctx: DrawingContext, state: DrawingState[Config]): Stream[Point] =
    state.config.expr.run(EvolutionEvaluator).get(Nil).unfold(RNG(state.seed))

  val initialConfig: Config = Config(new Expr[Point] {
    override def run[R[_]](alg: Evolution[RNGRepr, R, String, String]): R[RNGRepr[Point]] = {
      import alg.chain._, alg.bind._, alg.constants._
      fix(lambda("self", cons(point(double(0), double(0)), var0[RNGRepr[Point]])))
    }
  })

  override def configCodec: JsonCodec[Config] =
    DrawingJsonCodec
}
