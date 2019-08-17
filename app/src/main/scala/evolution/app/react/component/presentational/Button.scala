package evolution.app.react.component.presentational

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ Callback, ScalaComponent }

object Button {

  val component =
    ScalaComponent
      .builder[Callback]("Button")
      .render_PC { (callback, children) =>
        <.button(
          ^.className := "button is-black",
          ^.onClick --> callback,
          children
        )
      }
      .build
}
