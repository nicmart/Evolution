package evolution.app.react.component.presentational

import evolution.app.conf.Conf
import evolution.app.model.state.{DrawingState, RendererState}
import evolution.app.react.component.App.LayoutState
import evolution.app.react.component.Canvas
import evolution.app.react.component.control.{PlayToggle, RenderingSettings}
import evolution.geometry.Point
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CtorType, ScalaComponent}
import org.scalajs.dom
import org.scalajs.dom.raw.{Blob, URL}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import evolution.app.model.Drawing
import evolution.app.react.component.control.DrawingLoader

import scala.annotation.nowarn

object Page:
  type ReactComponent = Component[Props, Unit, Backend, CtorType.Props]

  def canvasInitializer: dom.html.Canvas => Unit =
    Conf.canvasInitializer

  case class Props(
      running: StateSnapshot[Boolean],
      layout: StateSnapshot[LayoutState],
      rendererState: StateSnapshot[RendererState],
      compilationResult: Either[String, Iterator[Point]],
      drawingState: StateSnapshot[DrawingState],
      selectedDrawing: StateSnapshot[Option[Drawing]],
      pointRate: Int,
      onShuffle: Callback,
      onReload: Callback,
      onFrameDraw: Callback,
      id: Int
  ):
    def canvasKey: String = (rendererState.value, drawingState.value, layout.value, id).hashCode().toString
    def code: StateSnapshot[String] =
      drawingState.zoomState(_.code)(code => state => state.copy(code = code))
    def sidebarWidth: StateSnapshot[Double] =
      layout.zoomState(_.sidebarWidth)(newWidth => layout => layout.copy(sidebarWidth = newWidth))
    def sidebarStatus: StateSnapshot[Boolean] =
      layout.zoomState(_.sidebarExpanded)(isExpanded => layout => layout.copy(sidebarExpanded = isExpanded))

  class Backend(canvasComponent: Canvas.ReactComponent):

    def render(props: Props): VdomElement =
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
              Button.component(props.onShuffle) {
                <.span(
                  ^.className := "icon",
                  <.i(^.className := "fas fa-random")
                )
              },
              Button.component(props.onReload) {
                <.span(
                  ^.className := "icon",
                  <.i(^.className := "fas fa-redo-alt")
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
            ^.className := "navbar-item",
            DrawingLoader.component(DrawingLoader.Props(props.selectedDrawing))
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
                props.compilationResult.toOption,
                props.onFrameDraw,
                props.running.value
              )
            )
          ),
          Sidebar.component(Sidebar.Props(props.sidebarStatus.value, props.sidebarWidth))(
            Editor.component(
              Editor.Props(
                props.code,
                props.compilationResult.swap.toOption
              )
            )
          )
        )
      )

    // https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/toBlob
    val downloadImage: Callback = Callback {
      val canvas = dom.document.getElementById(Canvas.canvasId).asInstanceOf[Html.Canvas]
      canvas.toBlob(blob => {
        val a = dom.document.createElement("a").asInstanceOf[Html.Anchor]
        a.download = "evolution.png"
        a.href = dom.URL.createObjectURL(blob)
        dom.document.body.appendChild(a)
        a.click()
        dom.document.body.removeChild(a)
      })
    }

  def component(canvasComponent: Canvas.ReactComponent): Page.ReactComponent =
    ScalaComponent
      .builder[Props]("Page")
      .backend[Backend](_ => new Backend(canvasComponent))
      .render(scope => scope.backend.render(scope.props))
      .build

object Html:
  @js.native
  @JSGlobal("Canvas")
  @nowarn
  class Canvas extends dom.html.Canvas:
    def toBlob(callback: js.Function1[Blob, ?], args: js.Any*): Blob = js.native

  @js.native
  @JSGlobal("Anchor")
  @nowarn
  class Anchor extends dom.html.Anchor:
    var download: String = js.native
