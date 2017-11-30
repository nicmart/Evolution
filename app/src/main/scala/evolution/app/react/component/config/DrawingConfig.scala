package evolution.app.react.component.config

import evolution.algebra.materializer.Materializer
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}

object DrawingConfig {
  case class Props[S, T, C](
    config: C,
    onChange: C => Callback,
    onStreamChange: (S => Stream[T]) => Callback
  )

  class Backend[S, T, C](
    definition: DrawingDefinition.Aux[T, C],
    context: DrawingContext,
    materializer: Materializer[S]
  )(bs: BackendScope[Props[S, T, C], Unit]) {
    def render(props: Props[S, T, C]): VdomElement = {
      definition.configComponent.element(
        ConfigComponent.Props(
          props.config,
          onChangeConfig,
          elements => <.div(elements.toTagMod)
        )
      )
    }

    def onChangeConfig(config: C): Callback = {
      for {
        props <- bs.props
        evolution = definition.evolution(config, context)
        streamer = (s: S) => materializer.materialize(s, evolution)
        _ <- props.onChange(config)
        _ <- props.onStreamChange(streamer)
      } yield ()
    }
  }

  def component[S, T](
    definition: DrawingDefinition[T],
    context: DrawingContext,
    materializer: Materializer[S]
  ) = ScalaComponent
    .builder[Props[S, T, definition.Config]]("drawing config")
    .backend(new Backend[S, T, definition.Config](definition, context, materializer)(_))
    .render(scope => scope.backend.render(scope.props))
    .build
}



