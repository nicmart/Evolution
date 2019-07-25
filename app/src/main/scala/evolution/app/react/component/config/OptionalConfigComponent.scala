package evolution.app.react.component.config

import evolution.app.react.component.config.ConfigComponent.instance
import evolution.app.react.component.config.instances.empty
import japgolly.scalajs.react.extra.StateSnapshot

object OptionalConfigComponent {
  def apply[T](component: ConfigComponent[T]): ConfigComponent[Option[T]] =
    instance("optional config") { (props, _) =>
      def tSnapshot(t: T): StateSnapshot[T] = props.zoomState(_ => t)(tt => _ => Some(tt))
      props.value.fold(empty)(t => component(tSnapshot(t))())
    }
}
