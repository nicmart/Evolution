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
import japgolly.scalajs.react.vdom.html_<^._

object dsl extends DrawingDefinition[Point] {
  import data.EvaluationModule._
  import expressionModule._
  val name = "drawing dsl"

  private val module = new FullModule[F]
  private val predefinedVars = List("left", "bottom", "right", "top")
  private val initialVarContext = new module.VarContext(predefinedVars)

  import module.ast.Type

  sealed abstract case class Config(serialisedExpr: String, expr: Expr[F[Point]])

  object Config {
    def from(serialisedExpr: String): Either[String, Config] = for {
      expr <- module.parse[Expr](serialisedExpr, Type.Evo(Type.Point), initialVarContext)
    } yield new Config(serialisedExpr, expr.asInstanceOf[Expr[F[Point]]]) {}
  }

  override val configComponent: ConfigComponent[Config] = {
    instance[Config]("drawing config") { (config2Snapshot, children) =>
      val stringSnapshot =
        config2Snapshot.zoomState[String](config2 => config2.serialisedExpr) { serialized => previousConfig =>
          {
            if (serialized == previousConfig.serialisedExpr) previousConfig
            else
              Config.from(serialized) match {
                case Right(cfg) =>
                  println(s"Parsed expression: ${cfg.expr}")
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
    data.EvaluationModule.materializeExpr(state.seed, bindPredefinedVars(ctx, state.config.expr))
  }

  private def bindPredefinedVars(ctx: DrawingContext, expr: Expr[F[Point]]): Expr[F[Point]] = {
    import expressionModule._
    Let[Double, F[Point]](
      "top",
      Dbl(ctx.top),
      Let[Double, F[Point]](
        "right",
        Dbl(ctx.right),
        Let[Double, F[Point]]("bottom", Dbl(ctx.bottom), Let[Double, F[Point]]("left", Dbl(ctx.left), expr)))
    )
  }

  val initialConfig: Config = Config.from("integrate(point(0, 0), <point>(uniform(-2, 2), uniform(-2, 2)))").right.get

  override def configCodec: JsonCodec[Config] =
    DrawingJsonCodec
}
