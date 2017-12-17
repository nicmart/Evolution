package evolution.app.react.component.config

import evolution.app.data.PointedSeq
import evolution.app.model.definition.{CompositeDefinitionConfig, DrawingDefinition}
import evolution.app.react.component.DrawingList
import evolution.app.react.component.config.ConfigComponent.instance
import evolution.app.react.component.presentational.styled.FormField
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._

object CompositeConfigComponent {
  def apply[T](drawings: PointedSeq[DrawingDefinition[T]]): ConfigComponent[CompositeDefinitionConfig[T]] = {
    val drawingListComponent = DrawingList.component[T]

    instance("composite config component") { (props, children) =>
      val config = props.value
      val innerComponent: ConfigComponent[config.InnerConfig] =
        config.definition.configComponent
      val innerConfig: config.InnerConfig = config.config

      val dropdown = FormField.component(FormField.Props("Drawing")) {
        <.div(
          drawingListComponent(
            props.zoomState(cfg => drawings.select(cfg.definition)) {
              drawings => cfg => CompositeDefinitionConfig[T, drawings.selected.Config](
                drawings.selected.initialConfig,
                drawings.selected
              )
            }
          )
        )
      }

      val innerConfigComp = innerComponent(
        StateSnapshot[config.InnerConfig](innerConfig)
          (newInnerConfig => props.setState(CompositeDefinitionConfig(newInnerConfig, config.definition)))
      )()

      <.div(^.className := "inner-config", dropdown, innerConfigComp)
    }
  }
}
