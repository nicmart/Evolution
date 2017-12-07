package evolution.app.model.state

import evolution.app.canvas.drawer._
import evolution.app.codec.JsonCodec
import evolution.app.model.context.DrawingContext
import io.circe.generic.auto._

final case class RendererState(
  iterations: Int,
  strokeSize: Int,
  trail: TrailSettings,
  offCanvasSettings: OffCanvasStrategy
)

object RendererState {
  implicit val jsonCodec: JsonCodec[RendererState] =
    JsonCodec[RendererState]
}

final case class TrailSettings(
  active: Boolean,
  opacity: Double
) {
  def decorate(frameDrawer: FrameDrawer, ctx: DrawingContext): FrameDrawer = {
    if (active) new ClearCanvasFrameDrawer(ctx, frameDrawer, RGBAColor(0, 0, 0, opacity))
    else frameDrawer
  }
}

sealed abstract class OffCanvasStrategy(name: String) {
  def decorate(pointDrawer: PointDrawer, ctx: DrawingContext): PointDrawer = this match {
    case InfiniteCanvas => pointDrawer
    case TorusCanvas => TorusPlaneDrawer(pointDrawer, ctx)
    case RealProjectivePlane => RealProjectivePlaneDrawer(pointDrawer, ctx)
  }
}
case object InfiniteCanvas extends OffCanvasStrategy("infinite")
case object TorusCanvas extends OffCanvasStrategy("torus")
case object RealProjectivePlane extends OffCanvasStrategy("real projective plane")

object RendererStateToFrameDrawer {
  def apply(f: (RendererState, DrawingContext) => PointDrawer)(state: RendererState, drawingContext: DrawingContext): FrameDrawer = {
    state.trail.decorate(
      BaseFrameDrawer(
        state.iterations,
        f(state, drawingContext)
      ),
      drawingContext
    )
  }
}

object RendererStateToPointDrawer {
  def apply(state: RendererState, ctx: DrawingContext): PointDrawer = {
    state.offCanvasSettings.decorate(
      FillRectPointDrawer(state.strokeSize),
      ctx
    )
  }
}
