package evolution.app.model.state

import evolution.app.canvas.drawer.*
import evolution.app.model.context.DrawingContext
import io.circe.*
import io.circe.generic.auto.*
import io.circe.syntax.*
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
    new JsonCodec[RendererState]:
      override def encode(state: RendererState): Json =
        state.asJson
      override def decode(json: Json): Option[RendererState] =
        json.as[RendererState].toOption

final case class TrailSettings(
    active: Boolean,
    opacity: Double
):
  def decorate(frameDrawer: FrameDrawer, ctx: DrawingContext): FrameDrawer =
    if (active) ClearCanvasFrameDrawer(ctx, frameDrawer, RGBAColor(0, 0, 0, opacity))
    else frameDrawer

enum OffCanvasStrategy:
  case Infinite
  case Torus
  case Projective

  def decorate(pointDrawer: PointDrawer, ctx: DrawingContext): PointDrawer = this match
    case Infinite   => pointDrawer
    case Torus      => TorusPlaneDrawer(pointDrawer, ctx)
    case Projective => RealProjectivePlaneDrawer(pointDrawer, ctx)

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
