package evolution.app.react.component.config

import japgolly.scalajs.react
import japgolly.scalajs.react.PropsChildren
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import evolution.app.react.component.presentational.TextArea

object ConfigComponent {

  def instance[C](name: String)(render: (StateSnapshot[C], PropsChildren) => VdomElement): ConfigComponent[C] =
    react.ScalaComponent.builder[StateSnapshot[C]](name).render_PC(render).build

  val textConfig: ConfigComponent[String] =
    instance[String]("string config") { (props, _) =>
      TextArea.component(props)
    }

}
