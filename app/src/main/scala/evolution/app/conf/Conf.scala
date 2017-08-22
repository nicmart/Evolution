package evolution.app.conf

import evolution.app.model.definition.DrawingListWithSelection
import evolution.app.portfolio._
import evolution.app.{CanvasInitializer, ColorCanvasInitializer, FullWindowCanvasInitializer}
import org.scalajs.dom

object Conf {
  lazy val canvasInitializer: CanvasInitializer =
    ColorCanvasInitializer("black")

  lazy val drawingList = DrawingListWithSelection(
    List(
      brownian,
      brownianWithRandomJumps,
      drops,
      waves,
      curlyRing
    ),
    brownian
  )
}
