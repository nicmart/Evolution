package evolution.app.react.component.presentational

import evolution.app.canvas.Drawer
import evolution.app.conf.Conf
import evolution.app.model.context.DrawingContext
import evolution.app.model.state.DrawingState
import evolution.app.react.component.Canvas
import evolution.app.react.component.config.DrawingConfig
import evolution.app.react.component.presentational.styled.HorizontalFormField
import evolution.geometry.Point
import japgolly.scalajs.react.{Callback, CallbackTo, CtorType, ScalaComponent}
import japgolly.scalajs.react.component.Scala.{BackendScope, Component}
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

import scala.util.Random

object Page {
  type ReactComponent[C] = Component[Props[C], Unit, Backend[C], CtorType.Props]

  def canvasInitializer: dom.html.Canvas => Unit =
    Conf.canvasInitializer

  case class Props[C](
    drawingContext: DrawingContext,
    drawer: Drawer,
    points: Stream[Point],
    drawingState: DrawingState[C],
    pointRate: Int,
    onConfigChange: C => Callback,
    onStreamChange: (Long => Stream[Point]) => Callback,
    onRefresh: Callback,
    onIterationsChange: Int => Callback,
    onFrameDraw: Callback
  ) {
    def canvasKey = (drawer.iterations, drawingState).hashCode().toString
  }

  class Backend[C](
    drawingConfig: DrawingConfig.ReactComponent[Long, Point, C]
  )(bs: BackendScope[Props[C], Unit]) {
    def render(props: Props[C]): VdomElement = {
      <.div(
        Navbar.component(
          <.div(^.className := "navbar-item is-hidden-touch", <.span(s"Seed: ${props.drawingState.seed.toHexString}")),
          <.div(^.className := "navbar-item is-hidden-touch", <.span(s"${props.pointRate} p/s")),
          <.div(
            ^.className := "navbar-item is-hidden-touch", Button.component(Button.Props(
              "Refresh",
              props.onRefresh
            )
            )
          ),
          <.div(
            ^.className := "navbar-item is-hidden-touch", HorizontalFormField.component(HorizontalFormField.Props(
              "Iterations",
              "",
              IntInputComponent(props.drawer.iterations, props.onIterationsChange)
            )
            )
          )
        ),
        <.div(
          ^.id := "page-content",
          Canvas.component.withKey(props.canvasKey)(Canvas.Props(
            props.drawingContext,
            canvasInitializer,
            props.drawer,
            props.points,
            props.onFrameDraw
          )
          ),
          Sidebar.component(
            drawingConfig(DrawingConfig.Props[Long, Point, C](
              props.drawingState.config,
              props.onConfigChange,
              props.onStreamChange
            )
            )
          )
        )
      )
    }
  }

  def component[C](drawingConfig: DrawingConfig.ReactComponent[Long, Point, C]): Page.ReactComponent[C] =
    ScalaComponent
      .builder[Props[C]]("Page")
      .backend[Backend[C]](scope => new Backend[C](drawingConfig)(scope))
      .render(scope => scope.backend.render(scope.props))
      .build
}
