package evolution.app.react.component

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

object NavbarComponent {

  case class Props(
    drawingDropDrown: VdomElement,
    elements: List[VdomElement]
  )

  class Backend(bs: BackendScope[Props, Unit]) {
    def render(props: Props): VdomElement = {
      <.nav(^.className := "navbar is-transparent",
        <.div(^.className := "navbar-brand",
          <.a(^.className := "navbar-item",
            <.h1(^.className := "title is-4",
              "Evolution"
            )
          )
        ),
        <.div(^.id := "navMenuExample", ^.className := "navbar-menu",
          <.div(^.className := "navbar-start"),
          <.div(^.className := "navbar-end",
            props.elements.map { element =>
              <.div(^.className := "navbar-item",
                element
              )
            }.toTagMod,
            <.div(^.className := "navbar-item",
              <.button(^.className := "button", ^.id := "restart", "Restart")
            ),
            <.div(^.className := "navbar-item",
              <.div(^.className := "field is-horizontal",
                <.div(^.className := "field-label is-normal",
                  <.label(^.className := "label", "Drawing")
                ),
                <.div(^.className := "field-body",
                  <.div(^.className := "field is-narrow",
                    <.div(^.className := "control",
                      <.div(^.className := "select is-fullwidth",
                        props.drawingDropDrown
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
  }

  val component = ScalaComponent.builder[Props]("Example")
      .renderBackend[Backend]
      .build
}