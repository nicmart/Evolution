package evolution.app.conf

import evolution.app.model.definition.DrawingDefinition
import evolution.app.portfolio._
import evolution.app.{CanvasInitializer, ColorCanvasInitializer}
import evolution.algebra.materializer.{Materializer, RNGMaterializer}
import evolution.algebra.interpreter.RNGInterpreter
import evolution.app.codec._
import evolution.app.model.context.DrawingContext
import evolution.app.model.state._
import evolution.app.react.pages.{LoadDrawingPage, MyPages, PageState}
import evolution.app.react.routing.Routing
import cats.implicits._
import evolution.app.canvas.drawer._
import evolution.app.data.PointedSeq
import evolution.app.model.counter.RateCounter
import evolution.app.model.state
import evolution.app.react.component.{App, Canvas}
import evolution.app.react.component.presentational.Page
import evolution.geometry.Point
import japgolly.scalajs.react.extra.router.Router

import scala.util.Random

object Conf {
  lazy val canvasInitializer: CanvasInitializer =
    ColorCanvasInitializer("black")

  lazy val drawings: List[DrawingDefinition[Point]] =
    List(
      line,
      circle,
      segments,
      brownian,
      brownianWithRandomJumps,
      drops,
      waves,
      circlesOnCircles,
      brownianStraight,
      dynamics,
      singlePoint,
      primes,
      dynamicRotation,
      nBodies,
      bouncing,
      lissajous,
      oscillator,
      dsl,
      new CompositeDrawingDefinition(PointedSeq(drawings, brownian))
    )

  lazy val innerDrawingList: PointedSeq[DrawingDefinition[Point]] =
    PointedSeq(drawings, brownian)

  lazy val drawingDefinition: DrawingDefinition[Point] =
    new DrawingListDefinition(innerDrawingList)

  type DrawingConfig = drawingDefinition.Config

  lazy val materializer: Materializer[Long] =
    RNGMaterializer(new RNGInterpreter)

  lazy val drawingStateCodec: JsonCodec[DrawingState[DrawingConfig]] =
    DrawingState.jsonCodec(drawingDefinition)

  lazy val pageStateCodec: JsonCodec[PageState[DrawingConfig]] =
    PageState.jsonCodec(drawingStateCodec)

  lazy val pageDrawingCodec: Codec[LoadDrawingPage[DrawingConfig], PageState[DrawingConfig]] =
    Codec.instance[LoadDrawingPage[DrawingConfig], PageState[DrawingConfig]](
      _.state,
      state => Some(LoadDrawingPage(state))
    )

  lazy val loadDrawingPageStringCodec: Codec[LoadDrawingPage[DrawingConfig], String] =
    pageDrawingCodec >>
      pageStateCodec >>
      JsonStringCodec >>
      StringByteCodec >>
      Base64Codec

  lazy val urlDelimiter = "#"

  lazy val initialPage: MyPages[DrawingConfig] =
    LoadDrawingPage(
      PageState(
        DrawingState(Random.nextLong(), drawingDefinition.initialConfig),
        RendererState(1000, 1, TrailSettings(false, 0.12), TorusCanvas)
      )
    )

  lazy val initialRateCounter: RateCounter =
    RateCounter.empty(1000)

  lazy val drawingConfComponent = drawingDefinition.configComponent

  lazy val points: (DrawingContext, DrawingState[DrawingConfig]) => Stream[Point] =
    (context, state) => materializer.materialize(state.seed, drawingDefinition.evolution(state.config, context))

  lazy val rendererStateToPointDrawer: (state.RendererState, DrawingContext) => PointDrawer =
    RendererStateToPointDrawer.apply

  lazy val rendererStateToFrameDrawer: (state.RendererState, DrawingContext) => FrameDrawer =
    RendererStateToFrameDrawer(rendererStateToPointDrawer)

  lazy val canvasComponent: Canvas.ReactComponent =
    Canvas.component(rendererStateToFrameDrawer)

  lazy val pageComponent: Page.ReactComponent[DrawingConfig] =
    Page.component[DrawingConfig](drawingConfComponent, canvasComponent)

  lazy val appComponent: App.ReactComponent[DrawingConfig] =
    App.component[DrawingConfig](points, canvasInitializer, initialRateCounter, pageComponent)

  lazy val routingConfig: Routing[DrawingConfig] =
    new Routing(urlDelimiter, appComponent, initialPage, loadDrawingPageStringCodec)

  lazy val router =
    Router(routingConfig.baseUrl, routingConfig.config)
}
