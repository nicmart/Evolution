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
import evolution.app.react.component.config.componentInstances._
import cats.implicits._
import evolution.app.conf.Conf.drawingDefinition
import evolution.app.react.component.App
import evolution.app.react.component.config.DrawingConfig
import evolution.app.react.component.presentational.Page
import evolution.geometry.Point
import japgolly.scalajs.react.{Callback, CtorType}
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.extra.router.Router
import org.scalajs.dom

import scala.util.Random

object Conf {
  lazy val canvasInitializer: CanvasInitializer =
    ColorCanvasInitializer("black")

  lazy val innerDrawingList: DrawingListWithSelection[Point] =
    DrawingListWithSelection(
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
        lissajous
      ),
      brownian
    )

  lazy val drawingDefinition: DrawingDefinition[Point] = new DrawingListDefinition(innerDrawingList)

  type DrawingConfig = drawingDefinition.Config

  lazy val drawingList: DrawingListWithSelection[Point] =
    DrawingListWithSelection(Nil, drawingDefinition)

  lazy val materializer: Materializer[Long] =
    RNGMaterializer(new RNGInterpreter)

  lazy val drawingContext: DrawingContext = {
    val document = dom.window.document
    DrawingContext(
      DrawingContext.CanvasSize(
        2 * Math.max(document.documentElement.clientWidth, dom.window.innerWidth).toInt,
        2 * Math.max(document.documentElement.clientHeight, dom.window.innerHeight).toInt
      )
    )
  }

  lazy val drawingStateCodec: JsonCodec[DrawingState[drawingDefinition.Config]] =
    DrawingState.jsonCodec(drawingDefinition)

  lazy val pageDrawingCodec: Codec[LoadDrawingPage[drawingDefinition.Config], DrawingState[DrawingConfig]] =
    Codec.instance[LoadDrawingPage[drawingDefinition.Config], DrawingState[DrawingConfig]](
      _.state,
      state => Some(LoadDrawingPage(state))
    )

  lazy val loadDrawingPageStringCodec: Codec[LoadDrawingPage[drawingDefinition.Config], String] =
    pageDrawingCodec >>
    drawingStateCodec >>
    JsonStringCodec >>
    StringByteCodec >>
    Base64Codec

  lazy val urlDelimiter = "#"

  lazy val initialPage: MyPages[drawingDefinition.Config] =
    LoadDrawingPage(
      DrawingState(
        Random.nextLong(),
        drawingDefinition.initialConfig
      )
    )

  lazy val drawingConfComponent =
    DrawingConfig.component[Long, Point](drawingDefinition, drawingContext, materializer)

  lazy val pageComponent: Page.ReactComponent[drawingDefinition.Config] =
    Page.component[drawingDefinition.Config](drawingConfComponent)

  lazy val appComponent: App.ReactComponent[drawingDefinition.Config] =
    App.component[drawingDefinition.Config](
      drawingDefinition,
      canvasInitializer,
      pageComponent
    )

  lazy val routingConfig: Routing[drawingDefinition.Config] =
    new Routing(
      urlDelimiter,
      appComponent,
      initialPage,
      loadDrawingPageStringCodec
    )

  lazy val router =
    Router(routingConfig.baseUrl, routingConfig.config)

  def areLoadableDrawingDifferent(drawing1: DrawingState[DrawingConfig], drawing2: DrawingState[DrawingConfig]): Boolean =
    drawingStateCodec.encode(drawing1) != drawingStateCodec.encode(drawing2)
}
