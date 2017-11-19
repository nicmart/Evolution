package evolution.app.react.pages

import evolution.app.codec.Codec
import evolution.app.model.state.LoadableDrawing

sealed trait MyPages

case object Home extends MyPages
case class LoadDrawingPage(loadableDrawing: LoadableDrawing) extends MyPages
case object NotFound extends MyPages

object LoadDrawingPage {
  class StringCodec(
    loadableDrawingCodec: Codec[LoadableDrawing, String]
  ) extends Codec[LoadDrawingPage, String] {

    override def encode(t: LoadDrawingPage): String =
      loadableDrawingCodec.encode(t.loadableDrawing)

    override def decode(r: String): Option[LoadDrawingPage] =
      loadableDrawingCodec.decode(r).map(LoadDrawingPage.apply)
  }
}
