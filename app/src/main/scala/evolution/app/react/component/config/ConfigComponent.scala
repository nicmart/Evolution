package evolution.app.react.component.config

import evolution.app.react.component.config.ConfigComponent.Props
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.{Callback, _}
import japgolly.scalajs.react.vdom.html_<^._

trait ConfigComponent[Config] {
  import ConfigComponent._
  def element(props: Props[Config]): List[VdomElement]
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

final case class ConfiguredComponent[Config](component: ConfigComponent[Config], config: Config) {
  def element(onChange: Config => Callback): VdomElement =
    <.div(
      component.element(Props(config, onChange)).toTagMod
    )

  def withConfig(config: Config): ConfiguredComponent[Config] =
    copy(config = config)
}


