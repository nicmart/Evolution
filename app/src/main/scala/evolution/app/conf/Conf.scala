package evolution.app.conf

import evolution.app.canvas.drawer.*
import evolution.app.codec.*
import evolution.app.model.context.DrawingContext
import evolution.app.model.counter.RateCounter
import evolution.app.model.state
import evolution.app.model.state.*
import evolution.app.react.component.presentational.Page
import evolution.app.react.component.{App, Canvas}
import evolution.app.react.pages.{LoadDrawingPage, MyPages, PageState, InterpretationOption}
import evolution.app.react.routing.Routing
import evolution.app.{CanvasInitializer, ColorCanvasInitializer}
import japgolly.scalajs.react.extra.router.Router

import scala.util.Random
import evolution.logging.Logger
import evolution.logging.NoOpLogger
import evolution.app.react.routing.DrawingPageUrl

object Conf:
  lazy val logger: Logger = NoOpLogger

  lazy val canvasInitializer: CanvasInitializer =
    ColorCanvasInitializer("black", "white")

  lazy val drawingStateCodec: JsonCodec[DrawingState] =
    DrawingState.jsonCodec

  lazy val rendererStateCodec: JsonCodec[RendererState] =
    RendererState.jsonCodec

  lazy val loadDrawingPageCodec: Codec[LoadDrawingPage, DrawingPageUrl] =
    pageDrawingCodec >> pageStateCodec

  lazy val pageStateCodec: Codec[PageState, DrawingPageUrl] =
    PageState.codec(stateCodec, InterpretationOption.codec)

  lazy val pageDrawingCodec: Codec[LoadDrawingPage, PageState] =
    Codec.instance[LoadDrawingPage, PageState](
      _.state,
      state => Some(LoadDrawingPage(state))
    )

  lazy val stateCodec: Codec[(DrawingState, RendererState), String] =
    StateJsonCodec >>
      JsonStringCodec >>
      StringByteCodec >>
      Base64Codec

  lazy val urlDelimiter = "#"

  lazy val defaultRendererState = RendererState(
    iterations = 1000,
    strokeSize = 1,
    resolutionFactor = 2,
    trail = TrailSettings(active = false, opacity = 0.12),
    offCanvasSettings = OffCanvasStrategy.Torus
  )

  lazy val initialPage: MyPages =
    LoadDrawingPage(
      PageState(
        DrawingState(Random.nextLong(), "integrate(point(0, 0), @point(uniform(-2, 2), uniform(-2, 2)))"),
        defaultRendererState,
        InterpretationOption.Eval
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
    Routing(urlDelimiter, appComponent, initialPage, loadDrawingPageCodec)

  lazy val router =
    Router(routingConfig.baseUrl, routingConfig.config)
