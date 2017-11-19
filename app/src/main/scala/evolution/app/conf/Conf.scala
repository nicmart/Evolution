package evolution.app.conf

import evolution.app.model.definition.DrawingListWithSelection
import evolution.app.portfolio._
import evolution.app.{CanvasInitializer, ColorCanvasInitializer}
import evolution.algebra.materializer.{Materializer, RNGMaterializer}
import evolution.algebra.interpreter.RNGInterpreter
import evolution.app.codec._
import evolution.app.model.configured._
import evolution.app.model.context.DrawingContext
import evolution.app.model.state.LoadableDrawing
import evolution.app.react.pages
import evolution.app.react.pages.{LoadDrawingPage, MyPages}
import evolution.geometry.Point
import org.scalajs.dom

import scala.util.Random

object Conf {
  lazy val canvasInitializer: CanvasInitializer =
    ColorCanvasInitializer("black")

  lazy val drawingList =
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
        bouncing
      ),
      brownian
    )

  lazy val materializer: Materializer[Long] =
    RNGMaterializer(new RNGInterpreter)

  lazy val definitionToComponent: DefinitionToComponent[Long, Point] =
    new MaterializerDefinitionToComponent(materializer)

  lazy val drawingContext: DrawingContext = {
    val document = dom.window.document
    DrawingContext(
      DrawingContext.CanvasSize(
        2 * Math.max(document.documentElement.clientWidth, dom.window.innerWidth).toInt,
        2 * Math.max(document.documentElement.clientHeight, dom.window.innerHeight).toInt
      )
    )
  }

  lazy val drawingComponentCodec: JsonCodec[DrawingComponent[Long, Point]] =
    new DrawingComponent.JsonCodec(
      drawingContext,
      drawingList.byName,
      definitionToComponent
    )

  lazy val loadableDrawingJsonCodec: JsonCodec[LoadableDrawing] =
    new LoadableDrawing.JsonCodec(drawingComponentCodec)

  lazy val pageDrawingCodec: Codec[LoadDrawingPage, LoadableDrawing] =
    Codec.instance[LoadDrawingPage, LoadableDrawing](
      _.loadableDrawing,
      drawing => Some(LoadDrawingPage(drawing))
    )

  lazy val loadDrawingPageStringCodec: Codec[LoadDrawingPage, String] =
    pageDrawingCodec >>
    loadableDrawingJsonCodec >>
    JsonStringCodec >>
    StringByteCodec >>
    Base64Codec

  lazy val initialPage: MyPages =
    LoadDrawingPage(
      LoadableDrawing(
        Random.nextLong(),
        definitionToComponent.toComponentWithInitialConfig(
          drawingList.current,
          drawingContext
        )
      )
    )

  def areLoadableDrawingEquivalent(drawing1: LoadableDrawing, drawing2: LoadableDrawing): Boolean =
    loadableDrawingJsonCodec.encode(drawing1) != loadableDrawingJsonCodec.encode(drawing2)
}
