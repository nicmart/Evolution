package evolution.app.react.component.config

import evolution.app.model.definition.CompositeDefinitionConfig
import evolution.app.react.component.config.ConfigComponent.instance
import evolution.app.react.underware.SnapshotUnderware
import japgolly.scalajs.react.vdom.html_<^._

object CompositeConfigComponent {
  def apply[T]: ConfigComponent[CompositeDefinitionConfig[T]] = {

    instance("composite config component") { (props, _) =>
      val config = props.value
      val innerComponent: ConfigComponent[config.InnerConfig] =
        config.definition.configComponent
      val innerConfig: config.InnerConfig = config.config

//      val dropdown = FormField.component(FormField.Props("Drawing")) {
//        <.div(drawingListComponent(props.zoomState(cfg => drawings.select(cfg.definition)) { drawings => _ =>
//          CompositeDefinitionConfig[T, drawings.selected.Config](drawings.selected.initialConfig, drawings.selected)
//        }))
//      }

      val innerConfigComp = innerComponent(
        SnapshotUnderware.simpleSnapshot[config.InnerConfig](innerConfig)(
          newInnerConfig => props.setState(CompositeDefinitionConfig(newInnerConfig, config.definition))
        )
      )()

      <.div(^.className := "inner-config", innerConfigComp)
    }
  }
}
