package evolution.app.react.pages

import evolution.app.codec.JsonCodec
import evolution.app.model.state.{ DrawingState, RendererState }
import evolution.app.model.Drawing
import io.circe.Json
import evolution.compiler.phases.materializing.Materializer
import evolution.compiler.impl.evaluation.EvalMaterializer
import evolution.compiler.impl.jsmaterialization.JsCodeMaterializer

sealed trait MyPages

case object Home extends MyPages
final case class LoadDrawingPage(state: PageState) extends MyPages
case object NotFound extends MyPages

final case class PageState(
  drawingState: DrawingState,
  rendererState: RendererState,
  materializer: MaterializationOption
)

object PageState {
  def jsonCodec(
    drawingStateCodec: JsonCodec[DrawingState],
    rendererStateCodec: JsonCodec[RendererState]
  ): JsonCodec[PageState] =
    new JsonCodec[PageState] {
      private val drawingStateField = "drawingState"
      private val rendererStateField = "rendererState"
      def decode(r: Json): Option[PageState] =
        for {
          drawingStateJson <- r.hcursor.get[Json](drawingStateField).toOption
          drawingState <- drawingStateCodec.decode(drawingStateJson)
          rendererStateJson <- r.hcursor.get[Json](rendererStateField).toOption
          rendererState <- rendererStateCodec.decode(rendererStateJson)
        } yield PageState(drawingState, rendererState, MaterializationOption.Eval)

      def encode(t: PageState): Json = Json.obj(
        drawingStateField -> drawingStateCodec.encode(t.drawingState),
        rendererStateField -> rendererStateCodec.encode(t.rendererState)
      )
    }

  def fromDrawing(drawing: Drawing): PageState =
    PageState(drawing.drawingState, drawing.rendererState, MaterializationOption.Eval)
}

sealed abstract class MaterializationOption(val materializer: Materializer)
object MaterializationOption {
  case object Eval extends MaterializationOption(EvalMaterializer)
  case object CodeGenerator extends MaterializationOption(JsCodeMaterializer)
}
