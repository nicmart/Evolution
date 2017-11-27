package evolution.app.react.component.config

import evolution.app.react.component.config.ConfigComponent.Props
import io.circe.{Decoder, Encoder}
import japgolly.scalajs
import japgolly.scalajs.react
import japgolly.scalajs.react.{Callback, CtorType, ScalaComponent}
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

trait ConfigComponent[Config] { self =>
  def element(props: Props[Config]): VdomElement
  val reactComponent: ConfigComponent.ReactComponent[Config] =
    ScalaComponent.builder[Props[Config]]("config")
        .render_P(element)
        .build
}

object ConfigComponent {
  type ReactComponent[Config] = Component[Props[Config], Unit, Unit, CtorType.Props]
  case class Props[Config](
    config: Config,
    callback: Config => Callback,
    render: List[VdomElement] => VdomElement
  )

  def prepend(head: VdomElement, render: List[VdomElement] => VdomElement)(tail: List[VdomElement]): VdomElement =
    render(head :: tail)

  def append(tail: List[VdomElement], render: List[VdomElement] => VdomElement)(head: VdomElement): VdomElement =
    render(head :: tail)

  def instance[Config](render: Props[Config] => VdomElement): ConfigComponent[Config] =
    new ConfigComponent[Config] {
      override def element(props: Props[Config]): VdomElement =
        render(props)
    }

  /**
    * Summoner method
    */
  def apply[Config](implicit component: ConfigComponent[Config]): ConfigComponent[Config]
    = component
}

