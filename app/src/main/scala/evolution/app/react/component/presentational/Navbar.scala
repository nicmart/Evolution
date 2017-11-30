package evolution.app.react.component.presentational

import japgolly.scalajs.react.{PropsChildren, ScalaComponent}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

object Navbar {

  class Backend(bs: BackendScope[Unit, Unit]) {
    def render(children: PropsChildren): VdomElement = {
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
            children
          )
        )
      )
    }
  }

  val component = ScalaComponent.builder[Unit]("navbar")
    .renderBackendWithChildren[Backend]
    .build
}