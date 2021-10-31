package evolution.app.model.state

import evolution.app.codec._
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._

import scala.util.Random

final case class DrawingState(seed: Long, code: String):
  def withNewSeed: DrawingState = copy(seed = Random.nextLong())

object DrawingState:
  val jsonCodec: JsonCodec[DrawingState] =
    new JsonCodec[DrawingState]:
      override def encode(state: DrawingState): Json =
        state.asJson
      override def decode(json: Json): Option[DrawingState] =
        json.as[DrawingState].toOption
