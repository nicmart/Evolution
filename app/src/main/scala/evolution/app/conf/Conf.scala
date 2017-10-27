package evolution.app.conf

import evolution.app.model.definition.DrawingListWithSelection
import evolution.app.portfolio._
import evolution.app.{CanvasInitializer, ColorCanvasInitializer}
import paint.evolution.materializer.{Materializer, RNGMaterializer}
import paint.evolution.algebra.interpreter.RNGInterpreter

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
    singlePoint
  )
  lazy val materializer: Materializer[Long] =
    RNGMaterializer(new RNGInterpreter)
}
