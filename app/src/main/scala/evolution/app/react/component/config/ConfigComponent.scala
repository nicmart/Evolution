package evolution.app.react.component.config

import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.{Callback, _}

trait ConfigComponent[Config] {
  import ConfigComponent._
  def component(props: Props[Config]): VdomElement
}

object ConfigComponent {
  case class Props[Config](
    config: Config,
    callback: Config => Callback
  )

  def instance[Config](render: Props[Config] => VdomElement): ConfigComponent[Config] =
    new ConfigComponent[Config] {
      override def component(props: Props[Config]): VdomElement =
        render(props)
    }

  /**
    * Summoner method
    */
  def apply[Config](implicit component: ConfigComponent[Config]): ConfigComponent[Config]
    = component
}


