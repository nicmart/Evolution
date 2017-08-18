package evolution.app.react.component.config

import evolution.app.react.component.config.ConfigComponent.Props
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.{Callback, _}

trait ConfigComponent[Config] {
  import ConfigComponent._
  def element(props: Props[Config]): VdomElement
}

object ConfigComponent {
  case class Props[Config](
    config: Config,
    callback: Config => Callback
  )

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

final case class ConfiguredComponent[Config](component: ConfigComponent[Config], config: Config) {
  def element(onChange: Config => Callback): VdomElement =
    component.element(Props(config, onChange))

  def withConfig(config: Config): ConfiguredComponent[Config] =
    copy(config = config)
}


