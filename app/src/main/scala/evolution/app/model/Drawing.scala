package evolution.app.model
import evolution.app.model.state.DrawingState
import evolution.app.model.state.RendererState

case class Drawing(
  title: Option[String],
  drawingState: DrawingState,
  rendererState: RendererState
)
