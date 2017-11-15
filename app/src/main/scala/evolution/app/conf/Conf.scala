package evolution.app.conf

import evolution.app.model.definition.{DrawingDefinition, DrawingListWithSelection}
import evolution.app.portfolio._
import evolution.app.{CanvasInitializer, ColorCanvasInitializer}
import evolution.algebra.materializer.{Materializer, RNGMaterializer}
import evolution.algebra.interpreter.RNGInterpreter
import evolution.app.ReactApp.{LoadDrawingPage, MyPages}
import evolution.app.model.configured._
import evolution.app.model.context.DrawingContext
import evolution.app.react.component.PageComponent.UrlState
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

  lazy val initialPage: MyPages =
    LoadDrawingPage(
      UrlState(
        Random.nextLong(),
        definitionToComponent.toComponent(
          drawingList.current,
          drawingContext
        )
      )
    )
}
