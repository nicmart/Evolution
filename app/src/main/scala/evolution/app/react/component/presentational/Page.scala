package evolution.app.react.component.presentational

import cats.Eval
import com.github.ghik.silencer.silent
import evolution.app.conf.Conf
import evolution.app.model.state.{ DrawingState, RendererState }
import evolution.app.react.component.App.LayoutState
import evolution.app.react.component.Canvas
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.control.{ PlayToggle, RenderingSettings }
import evolution.geometry.Point
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ Callback, CtorType, ScalaComponent }
import org.scalajs.dom
import org.scalajs.dom.raw.{ Blob, URL }

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import evolution.app.portfolio.dsl

object Page {
  type ReactComponent = Component[Props, Unit, Backend, CtorType.Props]

  def canvasInitializer: dom.html.Canvas => Unit =
    Conf.canvasInitializer

  case class Props(
    running: StateSnapshot[Boolean],
    layout: StateSnapshot[LayoutState],
    rendererState: StateSnapshot[RendererState],
    points: Eval[Iterator[Point]],
    drawingState: StateSnapshot[DrawingState],
    pointRate: Int,
    onRefresh: Callback,
    onFrameDraw: Callback
  ) {
    def canvasKey: String = (rendererState.value, drawingState.value, layout.value).hashCode().toString
    def config: StateSnapshot[dsl.Config] =
      drawingState.zoomState(_.config)(config => state => state.copy(config = config))
    def sidebarWidth: StateSnapshot[Double] =
      layout.zoomState(_.sidebarWidth)(newWidth => layout => layout.copy(sidebarWidth = newWidth))
    def sidebarStatus: StateSnapshot[Boolean] =
      layout.zoomState(_.sidebarExpanded)(isExpanded => layout => layout.copy(sidebarExpanded = isExpanded))
  }

  class Backend(
    drawingConfig: ConfigComponent[dsl.Config],
    canvasComponent: Canvas.ReactComponent
  ) {
    def render(props: Props): VdomElement = {
      <.div(
        Navbar.component(
          <.div(
            ^.className := "navbar-item is-hidden-touch",
            <.a(
              ^.href := "https://github.com/nicmart/Evolution",
              <.i(^.className := "fab fa-github fa-lg")
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
                  <.i(^.className := "fas fa-random")
                )
              },
              <.a(
                ^.id := "button-download",
                ^.className := "button is-black",
                ^.download := "evolution.png",
                ^.onClick --> downloadImage,
                <.span(
                  ^.className := "icon",
                  <.i(^.className := "fas fa-download")
                )
              )
            )
          ),
          <.div(
            ^.className := "navbar-item is-hidden-touch",
            RenderingSettings.component(props.rendererState)
          ),
          <.div(^.className := "navbar-item is-hidden-touch points-rate", <.span(s"${props.pointRate} p/s")),
          <.div(
            ^.className := "navbar-item",
            <.span(^.className := "is-size-7", s"${props.drawingState.value.seed.toHexString}")
          ),
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
                props.rendererState.value,
                props.points,
                props.onFrameDraw,
                props.running.value
              )
            )
          ),
          Sidebar.component(Sidebar.Props(props.sidebarStatus.value, props.sidebarWidth))(drawingConfig(props.config)())
        )
      )
    }

    // https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/toBlob
    val downloadImage: Callback = Callback {
      val canvas = dom.document.getElementById(Canvas.canvasId).asInstanceOf[Canvas]
      canvas.toBlob(blob => {
        val a = dom.document.createElement("a").asInstanceOf[Anchor]
        a.download = "evolution.png"
        a.href = URL.createObjectURL(blob)
        dom.document.body.appendChild(a)
        a.click()
        dom.document.body.removeChild(a)
      })
    }
  }

  def component(
    drawingConfig: ConfigComponent[dsl.Config],
    canvasComponent: Canvas.ReactComponent
  ): Page.ReactComponent =
    ScalaComponent
      .builder[Props]("Page")
      .backend[Backend](_ => new Backend(drawingConfig, canvasComponent))
      .render(scope => scope.backend.render(scope.props))
      .build
}

@js.native
@JSGlobal
@silent
class Canvas extends dom.html.Canvas {
  def toBlob(callback: js.Function1[Blob, _], args: js.Any*): Blob = js.native
}

@js.native
@JSGlobal
@silent
class Anchor extends dom.html.Anchor {
  var download: String = js.native
}
