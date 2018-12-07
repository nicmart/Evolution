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
import evolution.data
import evolution.geometry.Point
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.interpreter.{ EvolutionExpr, EvolutionSerializer }
import evolution.primitive.algebra.evolution.parser.EvolutionGrammar
import japgolly.scalajs.react.vdom.html_<^._

object dsl extends DrawingDefinition[Point] {
  import data.EvaluationModule._
  val name = "drawing dsl"

  type Expr[T] = Evolution.Expr[F, F[T]]

  private val serializer = new EvolutionSerializer[F]
  private val evolutionExpr = new EvolutionExpr[F]
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

  override def stream(ctx: DrawingContext, state: DrawingState[Config]): Iterator[Point] =
    data.EvaluationModule.materializeExpr[Point](state.seed, state.config.expr)

  val initialConfig: Config = Config(new Expr[Point] {
    override def run[R[_]](alg: Evolution[F, R]): R[F[Point]] = {
      import alg.chain._, alg.bind._, alg.constants._, alg.derived._, alg.distribution._
      integrate(point(double(0), double(0)), cartesian(uniform(double(-2), double(2)), uniform(double(-2), double(2))))
    }
  })

  override def configCodec: JsonCodec[Config] =
    DrawingJsonCodec
}
