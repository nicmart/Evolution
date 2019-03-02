package evolution.app.react.pages

import evolution.app.codec.JsonCodec
import evolution.app.model.state.{ DrawingState, RendererState }
import io.circe.{ Decoder, Encoder, ObjectEncoder }
import io.circe.generic.auto._

sealed trait MyPages[+C]

case object Home extends MyPages[Nothing]
final case class LoadDrawingPage[C](state: PageState[C]) extends MyPages[C]
case object NotFound extends MyPages[Nothing]

final case class PageState[C](
  drawingState: DrawingState[C],
  rendererState: RendererState
)

object PageState {
  def jsonCodec[C](implicit drawingStateCodec: JsonCodec[DrawingState[C]]): JsonCodec[PageState[C]] = {
    import JsonCodec._, RendererState._
    implicit val decoder: Decoder[DrawingState[C]] = toCirceDecoder(drawingStateCodec)
    implicit val encoder: Encoder[DrawingState[C]] = toCirceEncoder(drawingStateCodec)
    JsonCodec[PageState[C]]
  }
}
