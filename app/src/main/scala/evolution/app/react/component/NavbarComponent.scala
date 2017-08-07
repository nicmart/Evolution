package evolution.app.react.component

import evolution.app.portfolio.EvolutionPortfolio
import evolution.app.react.component.DrawingListComponent.{Backend, State}
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

object NavbarComponent {
    type State = Unit

    class Backend(bs: BackendScope[Unit, State]) {
        def render(state: State): VdomElement = {
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
                                                DrawingListComponent.component(
                                                    DrawingListComponent.Props(println(_))
                                                )
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

    val component = ScalaComponent.builder[Unit]("Example")
        .initialState(())
        .renderBackend[Backend]
        .build
}

/*
<nav class="navbar is-transparent">
    <div id="navMenuExample" class="navbar-menu">
        <div class="navbar-start">
        </div>
        <div class="navbar-end">
            <div class="navbar-item">
                <div class="field is-horizontal">
                    <div class="field-label is-normal">
                        <label class="label">Drawing</label>
                    </div>
                    <div class="field-body">
                        <div class="field is-narrow">
                            <div class="control">
                                <div class="select is-fullwidth">
                                    <div id="drawing">
                                    </div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
</nav>


 */