package evolution.app.model.state

import java.util.Base64

import evolution.app.codec
import evolution.app.codec.JsonCodec
import evolution.app.model.configured.DrawingComponent
import evolution.geometry.Point
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.auto._
import io.circe.parser.parse
import io.circe.syntax._

import scala.util.Try

/**
  * A drawing that can be loaded into a page
  */
case class LoadableDrawing(
  seed: Long,
  drawingComponent: DrawingComponent[Long, Point]
)

object LoadableDrawing {
  class JsonCodec(
    componentCodec: codec.JsonCodec[DrawingComponent[Long, Point]]
  ) extends codec.JsonCodec[LoadableDrawing] {

    implicit private val encoder: Encoder[DrawingComponent[Long, Point]] =
      JsonCodec.toCirceEncoder(componentCodec)

    implicit private val decoder: Decoder[DrawingComponent[Long, Point]] =
      JsonCodec.toCirceDecoder(componentCodec)

    override def encode(t: LoadableDrawing): Json =
      t.asJson

    override def decode(json: Json): Option[LoadableDrawing] =
      json.as[LoadableDrawing].toOption
  }
}
