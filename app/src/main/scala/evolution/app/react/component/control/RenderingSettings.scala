package evolution.app.react.component.control

import evolution.app.model.state._
import evolution.app.react.component.presentational.Select.Item
import evolution.app.react.component.presentational.{DoubleInputComponent, IntInputComponent, Select}
import evolution.app.react.component.presentational.SingleInput.Props
import evolution.app.react.component.presentational.styled.{FormField, HorizontalFormField}
import japgolly.scalajs.react
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

case class RenderingSettings(
  rendererState: RendererState,
  onChange: RendererState => Callback
) {
  def render = RenderingSettings.component(this)
}

object RenderingSettings {

  val offCanvasSettingComponent =
    Select.component[OffCanvasStrategy]

  class Backend(bs: BackendScope[RenderingSettings, Unit]) {
    def render(props: RenderingSettings): VdomElement = {
      import props._
      <.div(^.className := "dropdown is-hoverable",
        <.div(^.className := "dropdown-trigger",
          <.button(^.className := "button is-black", VdomAttr("aria-haspopup") := "true", VdomAttr("aria-controls") := "dropdown-menu",
            <.span("Rendering Settings"),
            <.span(^.className := "icon is-small",
              <.i(^.className := "fa fa-angle-down", VdomAttr("aria-hidden") :="true")
            )
          )
        ),
        <.div(^.className := "dropdown-menu", ^.id := "dropdown-menu", ^.role := "menu",
          <.div(^.className := "dropdown-content",
            <.div(^.className := "dropdown-item",
              FormField.component(FormField.Props("Off-canvas strategy")) {
                offCanvasSettingComponent(Select.Props[OffCanvasStrategy](
                  List(
                    Item("Infinite", InfiniteCanvas.toString, InfiniteCanvas),
                    Item("Torus", TorusCanvas.toString, TorusCanvas),
                    Item("Projective", RealProjectivePlane.toString, RealProjectivePlane)
                  ),
                  Item("", rendererState.offCanvasSettings.toString, rendererState.offCanvasSettings),
                  onChangeOffCanvasSetting(props)
                ))
              }
            ),
            <.div(^.className := "dropdown-item",
              FormField.component(FormField.Props("Stroke size")) {
                IntInputComponent(
                  rendererState.strokeSize,
                  onChange.compose(n => rendererState.copy(strokeSize = n))
                )
              }
            ),
            <.div(^.className := "dropdown-item",
              FormField.component(FormField.Props("Points per frame")) {
                IntInputComponent(
                  rendererState.iterations,
                  onChange.compose(n => rendererState.copy(iterations = n))
                )
              }
            ),
            <.div(^.className := "dropdown-item",
              <.label(^.className := "checkbox",
                <.input(
                  ^.`type` := "checkbox",
                  ^.checked := !rendererState.trail.active,
                  ^.onChange ==> onChangePersistOnCanvas(props)
                ),
                "Persist drawing on canvas"
              )
            ),
            <.div(^.className := "dropdown-item",
              FormField.component(FormField.Props("Trail opacity")) {
                DoubleInputComponent(
                  rendererState.trail.opacity,
                  onChange.compose { d =>
                    rendererState.copy(trail = rendererState.trail.copy(opacity = d))
                  }
                )
              }
            )
          )
        )
      )
    }

    private def onChangePersistOnCanvas(settings: RenderingSettings)(e: ReactEventFromInput): Callback = {
      val newTrailSettings = TrailSettings(
        !e.target.checked,
        settings.rendererState.trail.opacity
      )
      settings.onChange(settings.rendererState.copy(trail = newTrailSettings))
    }

    private def onChangeOffCanvasSetting(settings: RenderingSettings)(item: Item[OffCanvasStrategy]): Callback = {
      settings.onChange(settings.rendererState.copy(offCanvasSettings = item.value))
    }
  }

  private val component = ScalaComponent
    .builder[RenderingSettings]("rendering settings")
    .stateless
    .backend[Backend](new Backend(_))
    .render(scope => scope.backend.render(scope.props))
    .build
}
