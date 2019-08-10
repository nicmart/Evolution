package evolution.app.react.pages

import com.github.ghik.silencer.silent
import evolution.app.codec.JsonCodec
import evolution.app.model.state.{ DrawingState, RendererState }
import io.circe.generic.auto._
import io.circe.{ Decoder, Encoder }

sealed trait MyPages

case object Home extends MyPages
final case class LoadDrawingPage(state: PageState) extends MyPages
case object NotFound extends MyPages

final case class PageState(
  drawingState: DrawingState,
  rendererState: RendererState
)

object PageState {
  def jsonCodec(implicit drawingStateCodec: JsonCodec[DrawingState]): JsonCodec[PageState] = {
    import JsonCodec._
    @silent implicit val decoder: Decoder[DrawingState] = toCirceDecoder(drawingStateCodec)
    @silent implicit val encoder: Encoder[DrawingState] = toCirceEncoder(drawingStateCodec)
    JsonCodec[PageState]
  }
}
