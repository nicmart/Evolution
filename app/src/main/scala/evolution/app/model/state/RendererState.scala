package evolution.app.model.state

import evolution.app.canvas.drawer._
import evolution.app.model.context.DrawingContext
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import evolution.app.codec.JsonCodec

final case class RendererState(
    iterations: Int,
    strokeSize: Int,
    resolutionFactor: Int,
    trail: TrailSettings,
    offCanvasSettings: OffCanvasStrategy
)

object RendererState:

  val jsonCodec: JsonCodec[RendererState] =
    new JsonCodec[RendererState] {

      override def encode(state: RendererState): Json =
        state.asJson

      override def decode(json: Json): Option[RendererState] =
        json.as[RendererState].toOption
    }

final case class TrailSettings(
    active: Boolean,
    opacity: Double
):
  def decorate(frameDrawer: FrameDrawer, ctx: DrawingContext): FrameDrawer =
    if (active) new ClearCanvasFrameDrawer(ctx, frameDrawer, RGBAColor(0, 0, 0, opacity))
    else frameDrawer

sealed abstract class OffCanvasStrategy:
  def decorate(pointDrawer: PointDrawer, ctx: DrawingContext): PointDrawer = this match
    case InfiniteCanvas      => pointDrawer
    case TorusCanvas         => TorusPlaneDrawer(pointDrawer, ctx)
    case RealProjectivePlane => RealProjectivePlaneDrawer(pointDrawer, ctx)
case object InfiniteCanvas extends OffCanvasStrategy
case object TorusCanvas extends OffCanvasStrategy
case object RealProjectivePlane extends OffCanvasStrategy

object RendererStateToFrameDrawer:
  def apply(
      f: (RendererState, DrawingContext) => PointDrawer
  )(state: RendererState, drawingContext: DrawingContext): FrameDrawer =
    state.trail.decorate(
      BaseFrameDrawer(
        drawingContext,
        state.iterations,
        f(state, drawingContext)
      ),
      drawingContext
    )

object RendererStateToPointDrawer:
  def apply(state: RendererState, ctx: DrawingContext): PointDrawer =
    state.offCanvasSettings.decorate(
      FillRectPointDrawer(state.strokeSize),
      ctx
    )
