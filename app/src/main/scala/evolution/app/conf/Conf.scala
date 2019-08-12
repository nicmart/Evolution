package evolution.app.conf

import evolution.app.canvas.drawer._
import evolution.app.codec._
import evolution.app.model.context.DrawingContext
import evolution.app.model.counter.RateCounter
import evolution.app.model.state
import evolution.app.model.state._
import evolution.app.react.component.presentational.Page
import evolution.app.react.component.{ App, Canvas }
import evolution.app.react.pages.{ LoadDrawingPage, MyPages, PageState }
import evolution.app.react.routing.Routing
import evolution.app.{ CanvasInitializer, ColorCanvasInitializer }
import japgolly.scalajs.react.extra.router.Router

import scala.util.Random

object Conf {
  lazy val canvasInitializer: CanvasInitializer =
    ColorCanvasInitializer("black")

  lazy val drawingStateCodec: JsonCodec[DrawingState] =
    DrawingState.jsonCodec

  lazy val pageStateCodec: JsonCodec[PageState] =
    PageState.jsonCodec(drawingStateCodec)

  lazy val pageDrawingCodec: Codec[LoadDrawingPage, PageState] =
    Codec.instance[LoadDrawingPage, PageState](
      _.state,
      state => Some(LoadDrawingPage(state))
    )

  lazy val loadDrawingPageStringCodec: Codec[LoadDrawingPage, String] =
    pageDrawingCodec >>
      pageStateCodec >>
      JsonStringCodec >>
      StringByteCodec >>
      Base64Codec

  lazy val urlDelimiter = "#"

  lazy val defaultRendererState = RendererState(
    iterations = 1000,
    strokeSize = 1,
    resolutionFactor = 2,
    trail = TrailSettings(active = false, opacity = 0.12),
    offCanvasSettings = TorusCanvas
  )

  lazy val initialPage: MyPages =
    LoadDrawingPage(
      PageState(
        DrawingState(Random.nextLong(), "integrate(point(0, 0), @point(uniform(-2, 2), uniform(-2, 2)))"),
        defaultRendererState
      )
    )

  lazy val initialRateCounter: RateCounter =
    RateCounter.empty(1000)

  lazy val rendererStateToPointDrawer: (state.RendererState, DrawingContext) => PointDrawer =
    RendererStateToPointDrawer.apply

  lazy val rendererStateToFrameDrawer: (state.RendererState, DrawingContext) => FrameDrawer =
    RendererStateToFrameDrawer(rendererStateToPointDrawer)

  lazy val canvasComponent: Canvas.ReactComponent =
    Canvas.component(rendererStateToFrameDrawer)

  lazy val pageComponent: Page.ReactComponent =
    Page.component(canvasComponent)

  lazy val appComponent: App.ReactComponent =
    App.component(initialRateCounter, pageComponent)

  lazy val routingConfig: Routing =
    new Routing(urlDelimiter, appComponent, initialPage, loadDrawingPageStringCodec)

  lazy val router =
    Router(routingConfig.baseUrl, routingConfig.config)
}
