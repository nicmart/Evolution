package evolution.app.model.state

import evolution.app.codec._
import evolution.app.model.definition.DrawingDefinition
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.auto._
import io.circe.syntax._

import scala.util.Random

case class DrawingState[+C](
  seed: Long,
  config: C
) {
  def withNewSeed: DrawingState[C] = copy(seed = Random.nextLong())
}

object DrawingState {

  def jsonCodec[T](definition: DrawingDefinition[T]): JsonCodec[DrawingState[definition.Config]] =
    new JsonCodec[DrawingState[definition.Config]] {

      implicit private val encoder: Encoder[definition.Config] =
        JsonCodec.toCirceEncoder(definition.configCodec)

      implicit private val decoder: Decoder[definition.Config] =
        JsonCodec.toCirceDecoder(definition.configCodec)

      override def encode(state: DrawingState[definition.Config]): Json =
        state.asJson

      override def decode(json: Json): Option[DrawingState[definition.Config]] =
        json.as[DrawingState[definition.Config]].toOption
    }
}