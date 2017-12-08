package evolution.app.react.component.presentational

import evolution.app.conf.Conf
import evolution.app.model.context.DrawingContext
import evolution.app.model.state.{DrawingState, RendererState}
import evolution.app.react.component.Canvas
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.control.{PlayToggle, RenderingSettings}
import evolution.geometry.Point
import japgolly.scalajs.react.{Callback, CallbackTo, CtorType, ScalaComponent}
import japgolly.scalajs.react.component.Scala.{BackendScope, Component}
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
    drawingContext: DrawingContext,
    rendererState: StateSnapshot[RendererState],
    points: Stream[Point],
    drawingState: StateSnapshot[DrawingState[C]],
    pointRate: Int,
    onRefresh: Callback,
    onFrameDraw: Callback
  ) {
    def canvasKey: String =
      (rendererState.value, drawingState.value, drawingContext).hashCode().toString
  }

  class Backend[C](
    drawingConfig: ConfigComponent[C],
    canvasComponent: Canvas.ReactComponent
  )(bs: BackendScope[Props[C], Unit]) {
    def render(props: Props[C]): VdomElement = {
      val configState = props.drawingState.zoomState(_.config)(config => state => state.copy(config = config))
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
              PlayToggle.component(props.running),
              Button.component(props.onRefresh) {
                <.span(
                  ^.className := "icon",
                  <.i(^.className := "fa fa-random")
                )
              }
            )
          ),
          <.div(
            ^.className := "navbar-item is-hidden-touch",
            RenderingSettings.component(props.rendererState)
          ),
          <.div(^.className := "navbar-item is-hidden-touch points-rate", <.span(s"${props.pointRate} p/s")),
          <.div(^.className := "navbar-item",
            <.span(^.className := "is-size-7", s"${props.drawingState.value.seed.toHexString}")
          )
        ),
        <.div(
          ^.id := "page-content",
          canvasComponent.withKey(props.canvasKey)(Canvas.Props(
            props.drawingContext,
            canvasInitializer,
            props.rendererState.value,
            props.points,
            props.onFrameDraw,
            props.running.value
          )
          ),
          Sidebar.component(
            drawingConfig(ConfigComponent.Props[C](
              configState.value,
              configState.setState,
              elements => <.div(elements.toTagMod)
            ))
          )
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
