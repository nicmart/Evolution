package evolution.app.react.pages

import evolution.app.codec.Codec
import evolution.app.model.Drawing
import evolution.app.model.state.{DrawingState, RendererState}
import evolution.app.react.routing.DrawingPageUrl
import evolution.compiler.term.{OptimizedTermInterpreter, TermInterpreter}

sealed trait MyPages

case object Home extends MyPages
final case class LoadDrawingPage(state: PageState) extends MyPages
case object NotFound extends MyPages

final case class PageState(
    drawingState: DrawingState,
    rendererState: RendererState,
    materializer: InterpretationOption
)

object PageState:

  def codec(
      stateCodec: Codec[(DrawingState, RendererState), String],
      materializationOptionCodec: Codec[InterpretationOption, String]
  ): Codec[PageState, DrawingPageUrl] =
    new Codec[PageState, DrawingPageUrl]:
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

  def fromDrawing(drawing: Drawing): PageState =
    PageState(drawing.drawingState, drawing.rendererState, InterpretationOption.Eval)

sealed abstract class InterpretationOption(val interpreter: TermInterpreter)
object InterpretationOption:
  case object Eval extends InterpretationOption(OptimizedTermInterpreter)

  val codec = new Codec[InterpretationOption, String]:
    def encode(t: InterpretationOption): String = t match
      //case CodeGenerator => "js"
      case Eval => ""
    def decode(r: String): Option[InterpretationOption] = r match
      case "js" => None
      case _    => Some(Eval)
