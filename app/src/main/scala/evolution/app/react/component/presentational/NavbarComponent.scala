package evolution.app.react.component.presentational

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

object NavbarComponent {

  case class Props(elements: VdomElement*)

  class Backend(bs: BackendScope[Props, Unit]) {
    def render(props: Props): VdomElement = {
      <.nav(
        ^.className := "navbar is-transparent",
        <.div(
          ^.className := "navbar-brand is-hidden-touch",
          <.a(
            ^.className := "navbar-item",
            <.h1(
              ^.className := "title is-4",
              "Evolution"
            )
          )
        ),
        <.div(
          ^.id := "navMenuExample", ^.className := "navbar-menu is-active",
          <.div(^.className := "navbar-start"),
          <.div(
            ^.className := "navbar-end",
            props.elements.toTagMod
          )
        )
      )
    }
  }

  val component = ScalaComponent.builder[Props]("Example")
    .renderBackend[Backend]
    .build
}