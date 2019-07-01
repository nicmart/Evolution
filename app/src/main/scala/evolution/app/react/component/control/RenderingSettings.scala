package evolution.app.react.component.control

import evolution.app.data.PointedSeq
import evolution.app.model.state._
import evolution.app.react.component.presentational.Select.Item
import evolution.app.react.component.presentational.styled.FormField
import evolution.app.react.component.presentational.{ DoubleInputComponent, IntInputComponent, Select }
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ ScalaComponent, _ }

object RenderingSettings {

  val offCanvasSettingComponent =
    Select.component[OffCanvasStrategy]

  class Backend {
    def render(props: StateSnapshot[RendererState]): VdomElement = {
      <.div(
        ^.className := "dropdown is-hoverable",
        <.div(
          ^.className := "dropdown-trigger",
          <.button(
            ^.className := "button is-black",
            VdomAttr("aria-haspopup") := "true",
            VdomAttr("aria-controls") := "dropdown-menu",
            <.span("Rendering Settings"),
            <.span(
              ^.className := "icon is-small",
              <.i(^.className := "fa fa-angle-down", VdomAttr("aria-hidden") := "true"))
          )
        ),
        <.div(
          ^.className := "dropdown-menu",
          ^.id := "dropdown-menu",
          ^.role := "menu",
          <.div(
            ^.className := "dropdown-content",
            <.div(^.className := "dropdown-item", FormField.component(FormField.Props("Off-canvas strategy")) {
              offCanvasSettingComponent(offCanvasStrategyItems(props))
            }),
            <.div(^.className := "dropdown-item", FormField.component(FormField.Props("Stroke size")) {
              IntInputComponent.component(strokeSize(props))
            }),
            <.div(^.className := "dropdown-item", FormField.component(FormField.Props("Resolution Factor")) {
              IntInputComponent.component(resolutionFactor(props))
            }),
            <.div(^.className := "dropdown-item", FormField.component(FormField.Props("Points per frame")) {
              IntInputComponent.component(iterations(props))
            }),
            <.div(
              ^.className := "dropdown-item",
              <.label(
                ^.className := "checkbox",
                <.input(
                  ^.`type` := "checkbox",
                  ^.checked := !props.value.trail.active,
                  ^.onChange ==> ((b: Boolean) => isTrailActive(props).setState(b)).compose[ReactEventFromInput](e => !e.target.checked)
                ),
                "Persist drawing on canvas"
              )
            ),
            <.div(^.className := "dropdown-item", FormField.component(FormField.Props("Trail opacity")) {
              DoubleInputComponent.component(trailOpacity(props))
            })
          )
        )
      )
    }

    private def iterations(s: StateSnapshot[RendererState]): StateSnapshot[Int] =
      s.zoomState(_.iterations)(n => state => state.copy(iterations = n))

    private def strokeSize(s: StateSnapshot[RendererState]): StateSnapshot[Int] =
      s.zoomState(_.strokeSize)(n => state => state.copy(strokeSize = n))

    private def resolutionFactor(s: StateSnapshot[RendererState]): StateSnapshot[Int] =
      s.zoomState(_.resolutionFactor)(k => state => state.copy(resolutionFactor = k))

    private def trailSettings(s: StateSnapshot[RendererState]): StateSnapshot[TrailSettings] =
      s.zoomState(_.trail)(settings => state => state.copy(trail = settings))

    private def isTrailActive(s: StateSnapshot[RendererState]): StateSnapshot[Boolean] =
      trailSettings(s).zoomState(_.active)(active => state => state.copy(active = active))

    private def trailOpacity(s: StateSnapshot[RendererState]): StateSnapshot[Double] =
      trailSettings(s).zoomState(_.opacity)(opacity => state => state.copy(opacity = opacity))

    private def offCanvasSettings(s: StateSnapshot[RendererState]): StateSnapshot[OffCanvasStrategy] =
      s.zoomState(_.offCanvasSettings)(settings => state => state.copy(offCanvasSettings = settings))

    private def offCanvasStrategyItems(
      s: StateSnapshot[RendererState]): StateSnapshot[PointedSeq[Item[OffCanvasStrategy]]] = {
      def strategyToPointedSeq(strategy: OffCanvasStrategy): PointedSeq[Item[OffCanvasStrategy]] =
        PointedSeq(offCanvasStrategies, Item("", strategy.toString, strategy))
      offCanvasSettings(s).xmapState(strategyToPointedSeq)(_.selected.value)
    }
    private val offCanvasStrategies: Seq[Item[OffCanvasStrategy]] =
      List(
        Item("Infinite", InfiniteCanvas.toString, InfiniteCanvas),
        Item("Torus", TorusCanvas.toString, TorusCanvas),
        Item("Projective", RealProjectivePlane.toString, RealProjectivePlane)
      )
  }

  val component = ScalaComponent
    .builder[StateSnapshot[RendererState]]("rendering settings")
    .stateless
    .backend[Backend](_ => new Backend)
    .render(scope => scope.backend.render(scope.props))
    .build
}
