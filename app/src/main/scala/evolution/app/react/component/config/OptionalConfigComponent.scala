package evolution.app.react.component.config

import evolution.app.react.component.config.ConfigComponent.instance
import evolution.app.react.component.config.instances.empty
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._

object OptionalConfigComponent {
  def apply[T](component: ConfigComponent[T]): ConfigComponent[Option[T]] =
    instance("optional config") { (props, children) =>
      def tSnapshot(t: T): StateSnapshot[T] = props.zoomState(_ => t)(tt => opt => Some(tt))
      props.value.fold(empty)(t => component(tSnapshot(t))())
    }
}
