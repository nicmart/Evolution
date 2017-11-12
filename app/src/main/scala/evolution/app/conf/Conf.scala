package evolution.app.conf

import evolution.app.model.definition.{DrawingDefinition, DrawingListWithSelection}
import evolution.app.portfolio._
import evolution.app.{CanvasInitializer, ColorCanvasInitializer}
import evolution.algebra.materializer.{Materializer, RNGMaterializer}
import evolution.algebra.interpreter.RNGInterpreter
import evolution.app.model.configured._
import evolution.app.model.context.DrawingContext
import evolution.geometry.Point

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
}
