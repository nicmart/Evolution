package evolution.app.react.component.config

import japgolly.scalajs.react
import japgolly.scalajs.react.{Callback, PropsChildren}
import japgolly.scalajs.react.vdom.VdomElement

object ConfigComponent {
  case class Props[Config](
    config: Config,
    callback: Config => Callback,
    render: List[VdomElement] => VdomElement
  )

  def instance[C](name: String)(render: Props[C] => VdomElement): ConfigComponent[C] =
    react.ScalaComponent
      .builder[Props[C]](name)
      .stateless
      .render_P(render)
      .build

  def prepend(head: VdomElement, render: List[VdomElement] => VdomElement)(tail: List[VdomElement]): VdomElement =
    render(head :: tail)

  /**
    * Summoner method
    */
  def apply[Config](implicit component: ConfigComponent[Config]): ConfigComponent[Config]
    = component
}
