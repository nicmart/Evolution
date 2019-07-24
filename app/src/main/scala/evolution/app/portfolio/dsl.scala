package evolution.app.portfolio

import cats.implicits._
import com.github.ghik.silencer.silent
import evolution.app.codec.JsonCodec
import evolution.app.codec.config.DrawingJsonCodec
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.model.state.DrawingState
import evolution.app.react.component.config.{ ConfigComponent, instances }
import evolution.data
import evolution.geometry.Point
import evolution.language.{ FullModule, InstancesModule }
import japgolly.scalajs.react.{ Callback, PropsChildren }
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._

object dsl extends DrawingDefinition[Point] {
  import data.EvaluationModule._

  val name = "drawing dsl"

  private val predefinedVars = List("left", "bottom", "right", "top")
  private val module = new FullModule[EvoRepr] with InstancesModule[EvoRepr]
  import module.Typer.TypeBinding, module.TypeClasses.Qualified, module.Type
  private val initialVarContext = new module.VarContext(predefinedVars)
  private val predefinedVarsTypeBindings: module.Typer.TypeContext = Map(
    "left" -> TypeBinding.Variable("left", Qualified(Type.Dbl)),
    "right" -> TypeBinding.Variable("right", Qualified(Type.Dbl)),
    "top" -> TypeBinding.Variable("top", Qualified(Type.Dbl)),
    "bottom" -> TypeBinding.Variable("bottom", Qualified(Type.Dbl))
  )

  import module.{ Type, TypeInferenceOps, TypeInferenceResult, typeInference }

  // TODO I would really like to move expr into the state, but that cannot be done at the moment because
  // stream method needs to render the stream just using the Config. So the Expr HAS to go inside the config.
  case class Config(serialisedExpr: String, expr: Option[Expr[EvoRepr[Point]]])

  object Config {
    def from(serialisedExpr: String): (Config, State) = {
      val eitherExprOrError =
        module
          .parse[Expr, TypeInferenceResult](serialisedExpr, Type.Evo(Type.Point), initialVarContext)
          .map(_.asInstanceOf[Expr[EvoRepr[Point]]])
          .evaluateEither(predefinedVarsTypeBindings)
      (Config(serialisedExpr, eitherExprOrError.toOption), State(eitherExprOrError.swap.toOption))
    }
  }
 
  case class State(message: Option[String])

  class Backend(bs: BackendScope[StateSnapshot[Config], State]) {
    def render(snapshot: StateSnapshot[Config], state: State, @silent children: PropsChildren): VdomElement = {
      val stringSnapshot = StateSnapshot[String](snapshot.value.serialisedExpr) { case (Some(serialized), _) =>
        if (serialized == snapshot.value.serialisedExpr) Callback.empty
        else {
          val (config, state) = Config.from(serialized)
          snapshot.setState(config) >> bs.setState(state)
        }
      }

      val component: ConfigComponent[String] = instances.textConfig

      <.div(
        ^.className := "dsl-config",
        component.apply(stringSnapshot)(),
        <.div(
          ^.classSet(
            "dsl-feedback" -> true,
            "dsl-error" -> state.message.isDefined
          ),
          <.span(state.message.getOrElse("").toString)
        )
      )

    }
  }

  override val configComponent: ConfigComponent[Config] =
    japgolly.scalajs.react.ScalaComponent
      .builder[StateSnapshot[Config]]("DSL Config")
      // A bit hacky here, we re-parse the expression to load the error message. See TODO above
      .initialStateFromProps(props => Config.from(props.value.serialisedExpr)._2)
      .renderBackendWithChildren[Backend]
      .build

  override def stream(ctx: DrawingContext, state: DrawingState[Config]): Iterator[Point] = state.config.expr.map {
    expr =>
      data.EvaluationModule.materializeExpr(state.seed, bindPredefinedVars(ctx, expr))
  }.getOrElse(Iterator.empty)

  private def bindPredefinedVars(ctx: DrawingContext, expr: Expr[EvoRepr[Point]]): Expr[EvoRepr[Point]] = {
    import Expr._
    Let[Double, EvoRepr[Point]](
      "top",
      Dbl(ctx.top),
      Let[Double, EvoRepr[Point]](
        "right",
        Dbl(ctx.right),
        Let[Double, EvoRepr[Point]](
          "bottom",
          Dbl(ctx.bottom),
          Let[Double, EvoRepr[Point]]("left", Dbl(ctx.left), expr)))
    )
  }

  val initialConfig: Config = Config.from("integrate(point(0, 0), @point(uniform(-2, 2), uniform(-2, 2)))")._1

  override def configCodec: JsonCodec[Config] =
    DrawingJsonCodec
}
