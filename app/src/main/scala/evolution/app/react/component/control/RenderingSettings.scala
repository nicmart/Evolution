package evolution.app.react.component.control

import evolution.app.model.state._
import evolution.app.react.component.presentational.Select.Item
import evolution.app.react.component.presentational.{DoubleInputComponent, IntInputComponent, Select}
import evolution.app.react.component.presentational.styled.FormField
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot

object RenderingSettings {

  val offCanvasSettingComponent =
    Select.component[OffCanvasStrategy]

  class Backend(bs: BackendScope[StateSnapshot[RendererState], Unit]) {
    def render(props: StateSnapshot[RendererState]): VdomElement = {
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
                  Item("", props.value.offCanvasSettings.toString, props.value.offCanvasSettings),
                  offCanvasSettings(props).setState.compose(_.value)
                ))
              }
            ),
            <.div(^.className := "dropdown-item",
              FormField.component(FormField.Props("Stroke size")) {
                IntInputComponent(
                  strokeSize(props).value,
                  strokeSize(props).setState
                )
              }
            ),
            <.div(^.className := "dropdown-item",
              FormField.component(FormField.Props("Points per frame")) {
                IntInputComponent(
                  iterations(props).value,
                  iterations(props).setState
                )
              }
            ),
            <.div(^.className := "dropdown-item",
              <.label(^.className := "checkbox",
                <.input(
                  ^.`type` := "checkbox",
                  ^.checked := !props.value.trail.active,
                  ^.onChange ==> isTrailActive(props).setState.compose[ReactEventFromInput](e => !e.target.checked)
                ),
                "Persist drawing on canvas"
              )
            ),
            <.div(^.className := "dropdown-item",
              FormField.component(FormField.Props("Trail opacity")) {
                DoubleInputComponent(
                  props.value.trail.opacity,
                  trailOpacity(props).setState
                )
              }
            )
          )
        )
      )
    }

    private def iterations(s: StateSnapshot[RendererState]): StateSnapshot[Int] =
      s.zoomState(_.iterations)(n => state => state.copy(iterations = n))

    private def strokeSize(s: StateSnapshot[RendererState]): StateSnapshot[Int] =
      s.zoomState(_.strokeSize)(n => state => state.copy(strokeSize = n))

    private def trailSettings(s: StateSnapshot[RendererState]): StateSnapshot[TrailSettings] =
      s.zoomState(_.trail)(settings => state => state.copy(trail = settings))

    private def isTrailActive(s: StateSnapshot[RendererState]): StateSnapshot[Boolean] =
      trailSettings(s).zoomState(_.active)(active => state => state.copy(active = active))

    private def trailOpacity(s: StateSnapshot[RendererState]): StateSnapshot[Double] =
      trailSettings(s).zoomState(_.opacity)(opacity => state => state.copy(opacity = opacity))

    private def offCanvasSettings(s: StateSnapshot[RendererState]): StateSnapshot[OffCanvasStrategy] =
      s.zoomState(_.offCanvasSettings)(settings => state => state.copy(offCanvasSettings = settings))
  }

  val component = ScalaComponent
    .builder[StateSnapshot[RendererState]]("rendering settings")
    .stateless
    .backend[Backend](new Backend(_))
    .render(scope => scope.backend.render(scope.props))
    .build
}
