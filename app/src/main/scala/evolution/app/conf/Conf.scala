package evolution.app.conf

import evolution.app.model.definition.{DrawingDefinition, DrawingListWithSelection}
import evolution.app.portfolio._
import evolution.app.{CanvasInitializer, ColorCanvasInitializer}
import evolution.algebra.materializer.{Materializer, RNGMaterializer}
import evolution.algebra.interpreter.RNGInterpreter
import evolution.app.codec._
import evolution.app.model.context.DrawingContext
import evolution.app.model.state.DrawingState
import evolution.app.react.pages.{LoadDrawingPage, MyPages}
import evolution.app.react.routing.Routing
import cats.implicits._
import evolution.app.canvas.drawer._
import evolution.app.model.counter.RateCounter
import evolution.app.react.component.App
import evolution.app.react.component.config.DrawingConfig
import evolution.app.react.component.presentational.Page
import evolution.geometry.Point
import japgolly.scalajs.react.extra.router.Router

import scala.util.Random

object Conf {
  lazy val canvasInitializer: CanvasInitializer =
    ColorCanvasInitializer("black")

  lazy val drawings: List[DrawingDefinition[Point]] =
    List(
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
      oscillator
    )

  lazy val innerDrawingList: DrawingListWithSelection[Point] =
    DrawingListWithSelection(
      drawings :+ new CombinedDrawingsDefinition(DrawingListWithSelection(drawings, brownian)),
      brownian
    )

  lazy val drawingDefinition: DrawingDefinition[Point] =
    new DrawingListDefinition(innerDrawingList)

  type DrawingConfig = drawingDefinition.Config

  lazy val drawingList: DrawingListWithSelection[Point] =
    DrawingListWithSelection(Nil, drawingDefinition)

  lazy val materializer: Materializer[Long] =
    RNGMaterializer(new RNGInterpreter)

  lazy val drawingStateCodec: JsonCodec[DrawingState[DrawingConfig]] =
    DrawingState.jsonCodec(drawingDefinition)

  lazy val pageDrawingCodec: Codec[LoadDrawingPage[DrawingConfig], DrawingState[DrawingConfig]] =
    Codec.instance[LoadDrawingPage[DrawingConfig], DrawingState[DrawingConfig]](
      _.state,
      state => Some(LoadDrawingPage(state))
    )

  lazy val loadDrawingPageStringCodec: Codec[LoadDrawingPage[DrawingConfig], String] =
    pageDrawingCodec >>
    drawingStateCodec >>
    JsonStringCodec >>
    StringByteCodec >>
    Base64Codec

  lazy val urlDelimiter = "#"

  lazy val initialPage: MyPages[DrawingConfig] =
    LoadDrawingPage(
      DrawingState(
        Random.nextLong(),
        drawingDefinition.initialConfig
      )
    )

  def pointDrawerFromContext(drawingContext: DrawingContext): PointDrawer =
    TorusPlaneDrawer(
      FillRectDrawer(1),
      drawingContext
    )

  def frameDrawerFromContext(drawingContext: DrawingContext): BaseFrameDrawer =
    BaseFrameDrawer(
      iterations = 1000,
      pointDrawerFromContext(drawingContext),
      drawingContext
    )

  lazy val initialRateCounter: RateCounter =
    RateCounter.empty(1000)

  lazy val drawingConfComponent =
    DrawingConfig.component[Long, Point](drawingDefinition)

  lazy val points: (DrawingContext, DrawingState[DrawingConfig]) => Stream[Point] =
    (context, state) => materializer.materialize(state.seed, drawingDefinition.evolution(state.config, context))

  lazy val pageComponent: Page.ReactComponent[DrawingConfig] =
    Page.component[DrawingConfig](drawingConfComponent)

  lazy val appComponent: App.ReactComponent[DrawingConfig] =
    App.component[DrawingConfig](
      points,
      canvasInitializer,
      frameDrawerFromContext,
      initialRateCounter,
      pageComponent
    )


  lazy val routingConfig: Routing[DrawingConfig] =
    new Routing(
      urlDelimiter,
      appComponent,
      initialPage,
      loadDrawingPageStringCodec
    )

  lazy val router =
    Router(routingConfig.baseUrl, routingConfig.config)
}
