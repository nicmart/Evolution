package evolution.app.react.component.presentational

import cats.Eval
import evolution.app.conf.Conf
import evolution.app.model.context.DrawingContext
import evolution.app.model.state.{ DrawingState, RendererState }
import evolution.app.react.component.App.LayoutState
import evolution.app.react.component.Canvas
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.control.{ PlayToggle, RenderingSettings }
import evolution.geometry.Point
import japgolly.scalajs.react.{ Callback, CallbackTo, CtorType, ScalaComponent }
import japgolly.scalajs.react.component.Scala.{ BackendScope, Component }
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

object Page {
  type ReactComponent[C] = Component[Props[C], Unit, Backend[C], CtorType.Props]

  def canvasInitializer: dom.html.Canvas => Unit =
    Conf.canvasInitializer

  case class Props[C](
    running: StateSnapshot[Boolean],
    layout: StateSnapshot[LayoutState],
    renderer: StateSnapshot[RendererState],
    points: Eval[Iterator[Point]],
    drawingState: StateSnapshot[DrawingState[C]],
    pointRate: Int,
    onRefresh: Callback,
    onFrameDraw: Callback
  ) {
    def canvasKey: String = (renderer.value, drawingState.value, layout.value.drawingContext).hashCode().toString
    def config: StateSnapshot[C] = drawingState.zoomState(_.config)(config => state => state.copy(config = config))
    def sidebarWidth: StateSnapshot[Double] =
      layout.zoomState(_.sidebarWidth)(newWidth => layout => layout.copy(sidebarWidth = newWidth))
    def sidebarStatus: StateSnapshot[Boolean] =
      layout.zoomState(_.sidebarExpanded)(isExpanded => layout => layout.copy(sidebarExpanded = isExpanded))
  }

  class Backend[C](
    drawingConfig: ConfigComponent[C],
    canvasComponent: Canvas.ReactComponent
  )(bs: BackendScope[Props[C], Unit]) {
    def render(props: Props[C]): VdomElement = {
      <.div(
        Navbar.component(
          <.div(
            ^.className := "navbar-item is-hidden-touch",
            <.a(
              ^.href := "https://github.com/nicmart/Evolution",
              <.i(^.className := "fab fa-github fa-lg")
            )),
          <.div(
            ^.className := "navbar-item is-hidden-touch",
            <.div(
              ^.className := "buttons has-addons is-centered",
              PlayToggle.component(props.running),
              Button.component(props.onRefresh) {
                <.span(
                  ^.className := "icon",
                  <.i(^.className := "fas fa-random")
                )
              }
            )
          ),
          <.div(
            ^.className := "navbar-item is-hidden-touch",
            RenderingSettings.component(props.renderer)
          ),
          <.div(^.className := "navbar-item is-hidden-touch points-rate", <.span(s"${props.pointRate} p/s")),
          <.div(
            ^.className := "navbar-item",
            <.span(^.className := "is-size-7", s"${props.drawingState.value.seed.toHexString}")),
          <.div(
            ^.className := "navbar-end",
            <.div(
              ^.className := "navbar-item",
              Button.component(props.sidebarStatus.modState(!_)) {
                <.i(^.className := s"fas fa-angle-${if (props.sidebarStatus.value) "right" else "left"}")
              }
            )
          )
        ),
        <.div(
          ^.id := "page-content",
          ^.className := "columns is-gapless full-height",
          <.div(
            ^.id := "canvas-column",
            ^.className := "column is-paddingless full-height",
            canvasComponent.withKey(props.canvasKey)(
              Canvas.Props(
                props.layout.value.drawingContext,
                canvasInitializer,
                props.renderer.value,
                props.points,
                props.onFrameDraw,
                props.running.value
              ))
          ),
          Sidebar.component(Sidebar.Props(props.sidebarStatus.value, props.sidebarWidth))(drawingConfig(props.config)())
        )
      )
    }
  }

  def component[C](
    drawingConfig: ConfigComponent[C],
    canvasComponent: Canvas.ReactComponent
  ): Page.ReactComponent[C] =
    ScalaComponent
      .builder[Props[C]]("Page")
      .backend[Backend[C]](scope => new Backend[C](drawingConfig, canvasComponent)(scope))
      .render(scope => scope.backend.render(scope.props))
      .build
}
