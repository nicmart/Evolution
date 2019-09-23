package evolution.app.react.pages

import evolution.app.codec.Codec
import evolution.app.model.state.{ DrawingState, RendererState }
import evolution.app.model.Drawing
import evolution.compiler.phases.materializing.Materializer
import evolution.compiler.impl.evaluation.EvalMaterializer
import evolution.compiler.impl.jsmaterialization.JsCodeMaterializer
import evolution.app.react.routing.DrawingPageUrl

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

  def codec(
    stateCodec: Codec[(DrawingState, RendererState), String],
    materializationOptionCodec: Codec[MaterializationOption, String]
  ): Codec[PageState, DrawingPageUrl] =
    new Codec[PageState, DrawingPageUrl] {

      def decode(r: DrawingPageUrl): Option[PageState] =
        for {
          state <- stateCodec.decode(r.drawingSegment)
          (drawingState, rendererState) = state
          materialization <- materializationOptionCodec.decode(r.materializerSegment)
        } yield PageState(drawingState, rendererState, materialization)

      def encode(t: PageState): DrawingPageUrl = DrawingPageUrl(
        drawingSegment = stateCodec.encode((t.drawingState, t.rendererState)),
        materializerSegment = materializationOptionCodec.encode(t.materializer)
      )
    }

  def fromDrawing(drawing: Drawing): PageState =
    PageState(drawing.drawingState, drawing.rendererState, MaterializationOption.Eval)
}

sealed abstract class MaterializationOption(val materializer: Materializer)
object MaterializationOption {
  case object Eval extends MaterializationOption(EvalMaterializer)
  case object CodeGenerator extends MaterializationOption(JsCodeMaterializer)

  val codec = new Codec[MaterializationOption, String] {
    def encode(t: MaterializationOption): String = t match {
      case CodeGenerator => "js"
      case Eval          => ""
    }
    def decode(r: String): Option[MaterializationOption] = r match {
      case "js" => Some(CodeGenerator)
      case _    => Some(Eval)
    }
  }
}
