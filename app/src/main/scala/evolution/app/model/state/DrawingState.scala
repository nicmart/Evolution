package evolution.app.model.state

import evolution.app.codec._
import evolution.app.model.definition.DrawingDefinition
import io.circe.{ Decoder, Encoder, Json }
import io.circe.generic.auto._
import io.circe.syntax._
import com.github.ghik.silencer.silent

import scala.util.Random
import evolution.app.portfolio.dsl

final case class DrawingState(
  seed: Long,
  config: dsl.Config
) {
  def withNewSeed: DrawingState = copy(seed = Random.nextLong())
}

object DrawingState {

  def jsonCodec(definition: DrawingDefinition): JsonCodec[DrawingState] =
    new JsonCodec[DrawingState] {

      @silent
      implicit private val encoder: Encoder[dsl.Config] =
        JsonCodec.toCirceEncoder(definition.configCodec)

      @silent
      implicit private val decoder: Decoder[dsl.Config] =
        JsonCodec.toCirceDecoder(definition.configCodec)

      override def encode(state: DrawingState): Json =
        state.asJson

      override def decode(json: Json): Option[DrawingState] =
        json.as[DrawingState].toOption
    }
}
