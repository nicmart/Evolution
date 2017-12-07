package evolution.app.react.component.presentational

import evolution.app.canvas.drawer.BaseFrameDrawer
import evolution.app.conf.Conf
import evolution.app.model.context.DrawingContext
import evolution.app.model.state.{DrawingState, RendererState}
import evolution.app.react.component.Canvas
import evolution.app.react.component.config.DrawingConfig
import evolution.app.react.component.control.PlayToggle
import evolution.app.react.component.presentational.styled.HorizontalFormField
import evolution.geometry.Point
import japgolly.scalajs.react.{Callback, CallbackTo, CtorType, ScalaComponent}
import japgolly.scalajs.react.component.Scala.{BackendScope, Component}
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

object Page {
  type ReactComponent[C] = Component[Props[C], Unit, Backend[C], CtorType.Props]

  def canvasInitializer: dom.html.Canvas => Unit =
    Conf.canvasInitializer

  case class Props[C](
    running: Boolean,
    drawingContext: DrawingContext,
    rendererState: RendererState,
    points: Stream[Point],
    drawingState: DrawingState[C],
    pointRate: Int,
    onRunningToggleChange: Boolean => Callback,
    onConfigChange: C => Callback,
    onRefresh: Callback,
    onIterationsChange: Int => Callback,
    onFrameDraw: Callback
  ) {
    def canvasKey = (rendererState, drawingState, drawingContext).hashCode().toString
  }

  class Backend[C](
    drawingConfig: DrawingConfig.ReactComponent[Long, Point, C],
    canvasComponent: Canvas.ReactComponent
  )(bs: BackendScope[Props[C], Unit]) {
    def render(props: Props[C]): VdomElement = {
      <.div(
        Navbar.component(
          <.div(^.className := "navbar-item is-hidden-touch",
            <.a(
              ^.href := "https://github.com/nicmart/Evolution",
              <.span(
                ^.className := "icon",
                <.i(^.className := "fa fa-lg fa-github")
              )
            )
          ),
          <.div(
            ^.className := "navbar-item is-hidden-touch",
            <.div(
              ^.className := "buttons has-addons is-centered",
              PlayToggle.component(PlayToggle.Props(props.running, props.onRunningToggleChange)),
              Button.component(props.onRefresh) {
                <.span(
                  ^.className := "icon",
                  <.i(^.className := "fa fa-random")
                )
              }
            )
          ),
          <.div(
            ^.className := "navbar-item is-hidden-touch", HorizontalFormField.component(HorizontalFormField.Props(
              "Iterations",
              "",
              IntInputComponent(props.rendererState.iterations, props.onIterationsChange)
            )
            )
          ),
          <.div(^.className := "navbar-item is-hidden-touch points-rate", <.span(s"${props.pointRate} p/s")),
          <.div(^.className := "navbar-item",
            <.span(^.className := "is-size-7", s"${props.drawingState.seed.toHexString}")
          )
        ),
        <.div(
          ^.id := "page-content",
          canvasComponent.withKey(props.canvasKey)(Canvas.Props(
            props.drawingContext,
            canvasInitializer,
            props.rendererState,
            props.points,
            props.onFrameDraw,
            props.running
          )
          ),
          Sidebar.component(
            drawingConfig(DrawingConfig.Props[Long, Point, C](
              props.drawingState.config,
              props.onConfigChange
            )
            )
          )
        )
      )
    }
  }

  def component[C](
    drawingConfig: DrawingConfig.ReactComponent[Long, Point, C],
    canvasComponent: Canvas.ReactComponent
  ): Page.ReactComponent[C] =
    ScalaComponent
      .builder[Props[C]]("Page")
      .backend[Backend[C]](scope => new Backend[C](drawingConfig, canvasComponent)(scope))
      .render(scope => scope.backend.render(scope.props))
      .build
}
