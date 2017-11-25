package evolution.app.react.component.config

import evolution.app.react.component.config.ConfigComponent.Props
import io.circe.{Decoder, Encoder}
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

trait ConfigComponent[Config] { self =>
  def element(props: Props[Config]): List[VdomElement]

  def inmap[C2](f: Config => C2, g: C2 => Config): ConfigComponent[C2] =
    new ConfigComponent[C2] {
      override def element(props: Props[C2]): List[VdomElement] = {
        val propsC1 = Props[Config](
          g(props.config),
          f andThen props.callback
        )
        self.element(propsC1)
      }
    }
}

object ConfigComponent {

  case class Props[Config](
    config: Config,
    callback: Config => Callback
  )

  def instance[Config](render: Props[Config] => List[VdomElement]): ConfigComponent[Config] =
    new ConfigComponent[Config] {
      override def element(props: Props[Config]): List[VdomElement] =
        render(props)
    }

  /**
    * Summoner method
    */
  def apply[Config](implicit component: ConfigComponent[Config]): ConfigComponent[Config]
    = component
}

