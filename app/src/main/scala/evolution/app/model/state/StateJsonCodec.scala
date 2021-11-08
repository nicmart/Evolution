package evolution.app.model.state

import evolution.app.codec.*
import io.circe.Json
import io.circe.generic.auto.*
import io.circe.syntax.*

object StateJsonCodec extends JsonCodec[(DrawingState, RendererState)]:
  private val drawingStateField = "drawingState"
  private val rendererStateField = "rendererState"

  def decode(r: Json): Option[(DrawingState, RendererState)] =
    for
      drawingState <- r.hcursor.get[DrawingState](drawingStateField).toOption
      rendererState <- r.hcursor.get[RendererState](rendererStateField).toOption
    yield (drawingState, rendererState)

  def encode(t: (DrawingState, RendererState)): Json = Json.obj(
    drawingStateField -> t._1.asJson,
    rendererStateField -> t._2.asJson
  )
