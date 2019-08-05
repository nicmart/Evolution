package evolution.app.react.component.config

import evolution.app.react.component.config.ConfigComponent.instance
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._

object SeqConfigComponent {
  def apply[T](innerComponent: ConfigComponent[T]): ConfigComponent[Seq[T]] =
    instance[Seq[T]]("sequence config") { (props, _) =>
      val components = for {
        i <- props.value.indices
        snapshot = tSnapshot(props)(i)
        component = innerComponent(snapshot)()
      } yield component
      <.div(components.toTagMod)
    }

  private def tSnapshot[T](props: StateSnapshot[Seq[T]])(index: Int): StateSnapshot[T] =
    props.zoomState(_.apply(index))(t => ts => ts.updated(index, t))
}