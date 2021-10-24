package evolution.app.react.component.config

import japgolly.scalajs.react
import japgolly.scalajs.react.{CtorType, PropsChildren}
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import evolution.app.react.component.presentational.TextArea
import japgolly.scalajs.react.component.Scala.Component

type ConfigComponent[T] = Component[StateSnapshot[T], Any, Any, CtorType.PropsAndChildren]

object ConfigComponent:

  def instance[C](name: String)(render: (StateSnapshot[C], PropsChildren) => VdomElement): ConfigComponent[C] =
    // TODO scala3
    react.ScalaComponent.builder[StateSnapshot[C]](name).render_PC(render).build.asInstanceOf

  val textConfig: ConfigComponent[String] =
    instance[String]("string config") { (props, _) =>
      TextArea.component(props)
    }

