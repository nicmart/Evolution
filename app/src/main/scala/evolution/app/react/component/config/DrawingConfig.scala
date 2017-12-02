package evolution.app.react.component.config

import evolution.app.model.definition.DrawingDefinition
import japgolly.scalajs.react.component.Scala.{BackendScope, Component}
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CtorType, ScalaComponent}

object DrawingConfig {
  type ReactComponent[S, T, C] =
    Component[Props[S, T, C], Unit, Backend[S, T, C], CtorType.Props]

  case class Props[S, T, C](
    config: C,
    onChange: C => Callback
  )

  class Backend[S, T, C](
    definition: DrawingDefinition.Aux[T, C]
  )(bs: BackendScope[Props[S, T, C], Unit]) {
    def render(props: Props[S, T, C]): VdomElement = {
      definition.configComponent.element(
        ConfigComponent.Props(
          props.config,
          props.onChange,
          elements => <.div(elements.toTagMod)
        )
      )
    }
  }

  def component[S, T](
    definition: DrawingDefinition[T]
  ): ReactComponent[S, T, definition.Config] = ScalaComponent
    .builder[Props[S, T, definition.Config]]("drawing config")
    .backend(new Backend[S, T, definition.Config](definition)(_))
    .render(scope => scope.backend.render(scope.props))
    .build
}



