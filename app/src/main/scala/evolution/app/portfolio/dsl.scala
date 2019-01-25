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
import evolution.data.EvaluationModule
import evolution.geometry.Point
import evolution.primitive.FullModule
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.interpreter.{
  DesugarEvolutionSerializer,
  EvolutionExpr,
  EvolutionSerializer
}
import evolution.primitive.algebra.evolution.parser.EvolutionGrammar
import japgolly.scalajs.react.vdom.html_<^._

object dsl extends DrawingDefinition[Point] {
  import data.EvaluationModule._
  val name = "drawing dsl"

  type Expr[T] = Evolution.Expr[F, F[T]]

  private val initialContext = List("left", "bottom", "right", "top")
  private val serializer = new EvolutionSerializer[F]
  private val desugaringSerializer = new DesugarEvolutionSerializer[F]
  private val evolutionExpr = new EvolutionExpr[F]
  private val grammar = EvolutionGrammar.parserGrammar(evolutionExpr)
  private val algebraParser = grammar.evolutionOfPoints
  private val stringParser = algebraParser.parser(initialContext)

  val module = new FullModule[F]
  import module.ast.Type
  case class Config(expr: Expr[Point])

  override val configComponent: ConfigComponent[Config] = {
    instance[Config]("drawing config") { (config2Snapshot, children) =>
      val stringSnapshot =
        config2Snapshot.zoomState[String](config2 => config2.expr.run(serializer)(initialContext)) {
          serialized => previousConfig =>
            {
              println("parsing inside configComponent")
              module.parse(serialized, Type.Evo(Type.Point), evolutionExpr) match {
                case Right(expr) =>
                  println(s"Parsed expression: ${expr.run(serializer)(initialContext)}")
                  println(s"Desugared expression: ${expr.run(desugaringSerializer)(initialContext)}")
                  Config(expr.asInstanceOf[Expr[Point]])
                case Left(error) =>
                  println(error)
                  previousConfig
              }
            }
        }
      val component: ConfigComponent[String] = instances.textConfig
      component.apply(stringSnapshot)()
    }
  }

  override def stream(ctx: DrawingContext, state: DrawingState[Config]): Iterator[Point] = {
    println(s"Full expression: ${bindPredefinedVars(ctx, state.config.expr).run(serializer)(Nil)}")
    println(s"Full desugared expression: ${bindPredefinedVars(ctx, state.config.expr).run(desugaringSerializer)(Nil)}")
    data.EvaluationModule.materializeExpr[Point](state.seed, bindPredefinedVars(ctx, state.config.expr))
  }

  private def bindPredefinedVars(ctx: DrawingContext, expr: Expr[Point]): Expr[Point] =
    new Expr[Point] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[Point]] = {
        import alg.bind._, alg.constants.double
        let[Double, F[Point]](
          "top",
          double(ctx.top),
          let[Double, F[Point]](
            "right",
            double(ctx.right),
            let[Double, F[Point]](
              "bottom",
              double(ctx.bottom),
              let[Double, F[Point]]("left", double(ctx.left), expr.run(alg))))
        )
      }
    }

  val initialConfig: Config = Config(new Expr[Point] {
    override def run[R[_]](alg: Evolution[F, R]): R[F[Point]] = {
      import alg.chain._, alg.bind._, alg.constants._, alg.derived._, alg.distribution._
      integrate(point(double(0), double(0)), cartesian(uniform(double(-2), double(2)), uniform(double(-2), double(2))))
      empty
    }
  })

  override def configCodec: JsonCodec[Config] =
    DrawingJsonCodec
}
