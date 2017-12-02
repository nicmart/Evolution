package evolution.app.react.component.presentational


import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}

object Button {

  final case class Props(label: String, callback: Callback)

  val component =
    ScalaComponent.builder[Props]("Button")
      .render_P { props =>
        <.button(
          ^.className := "button",
          ^.onClick --> props.callback,
          props.label
        )
      }
      .build
}
