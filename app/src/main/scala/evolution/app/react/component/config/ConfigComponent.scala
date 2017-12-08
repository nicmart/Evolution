package evolution.app.react.component.config

import japgolly.scalajs.react
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.{Callback, PropsChildren}
import japgolly.scalajs.react.vdom.VdomElement

object ConfigComponent {

  def instance[C](name: String)(render: (StateSnapshot[C], PropsChildren) => VdomElement): ConfigComponent[C] =
    react.ScalaComponent
      .builder[StateSnapshot[C]](name)
      .render_PC(render)
      .build

  /**
    * Summoner method
    */
  def apply[Config](implicit component: ConfigComponent[Config]): ConfigComponent[Config]
    = component
}
