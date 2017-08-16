package evolution.app.model.legacy

import evolution.app.canvas.CanvasSize
import paint.evolution.Evolution

import scala.collection.immutable.ListMap

final case class Drawing[T](
  name: String,
  evolution: CanvasSize => Evolution[T]
)

final case class DrawingList[T] private(drawings: ListMap[String, Drawing[T]], selected: Option[Drawing[T]]) {
  def withDrawing(drawing: Drawing[T], select: Boolean = false): DrawingList[T] = {
    val newSelected = if (select) Some(drawing) else selected
    DrawingList(drawings.updated(drawing.name, drawing), newSelected)
  }

  def withSelected(drawingName: String): DrawingList[T] = {
    val newSelected: Option[Drawing[T]] = drawing(drawingName).fold(selected)(drawing => Some(drawing))
    DrawingList(drawings, newSelected)
  }

  def drawing(name: String): Option[Drawing[T]] =
    drawings.get(name)
}

object DrawingList {
  def empty[T]: DrawingList[T] = DrawingList(ListMap.empty, None)
}
