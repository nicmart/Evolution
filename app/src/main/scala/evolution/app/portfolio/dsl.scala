package evolution.app.portfolio

import cats.Id
import evolution.app.codec.JsonCodec
import evolution.app.codec.config.DrawingJsonCodec
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.model.state.DrawingState
import evolution.app.react.component.config.ConfigComponent.instance
import evolution.app.react.component.config.{ ConfigComponent, instances }
import evolution.data
import evolution.geometry.Point
import evolution.primitive.FullModule
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.interpreter.{
  DeBruijnEvolutionSerializer,
  DesugarEvolutionSerializer,
  EvolutionExpr,
  EvolutionSerializer
}
import japgolly.scalajs.react.vdom.html_<^._

object dsl extends DrawingDefinition[Point] {
  import data.EvaluationModule._
  val name = "drawing dsl"

  type Expr[T] = Evolution.Expr[F, F[T]]

  private val module = new FullModule[F]
  private val predefinedVars = List("left", "bottom", "right", "top")
  private val initialVarContext = new module.VarContext(predefinedVars)
  private val serializer = new EvolutionSerializer[F]
  private val desugaringSerializer = new DesugarEvolutionSerializer[F]
  private val deBrujinSerializer = new DeBruijnEvolutionSerializer[F]
  private val evolutionExpr = new EvolutionExpr[F]

  import module.ast.Type

  case class ConfigX(expr: Expr[Point])

  sealed abstract case class Config(serialisedExpr: String, expr: Expr[Point])

  object Config {
    def from(serialisedExpr: String): Either[String, Config] = for {
      expr <- module.parse(serialisedExpr, Type.Evo(Type.Point), evolutionExpr, initialVarContext)
    } yield new Config(serialisedExpr, expr.asInstanceOf[Expr[Point]]) {}
  }

  override val configComponent: ConfigComponent[Config] = {
    instance[Config]("drawing config") { (config2Snapshot, children) =>
      val stringSnapshot =
        config2Snapshot.zoomState[String](config2 => config2.serialisedExpr) { serialized => previousConfig =>
          {
            println("parsing inside configComponent")
            Config.from(serialized) match {
              case Right(cfg) =>
                println(s"Parsed expression: ${cfg.expr.run(serializer)(predefinedVars)}")
                println(
                  s"Parsed expression with De Brujn indexes:\n${cfg.expr.run(deBrujinSerializer)(predefinedVars)}")
                println(s"Desugared expression: ${cfg.expr.run(desugaringSerializer)(predefinedVars)}")
                cfg
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

  val initialConfig: Config = Config.from("constant(point(0, 0))").right.get

  override def configCodec: JsonCodec[Config] =
    DrawingJsonCodec
}
