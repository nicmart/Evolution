package evolution.app.react.component.presentational

import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ PropsChildren, ScalaComponent }

object Navbar {

  class Backend {
    def render(children: PropsChildren): VdomElement = {
      <.nav(
        ^.className := "navbar is-transparent has-text-white",
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
          ^.id := "navMenuExample",
          ^.className := "navbar-menu is-active",
          <.div(^.className := "navbar-start"),
          <.div(
            ^.className := "navbar-end",
            children
          )
        )
      )
    }
  }

  val component = ScalaComponent.builder[Unit]("navbar").renderBackendWithChildren[Backend].build
}
