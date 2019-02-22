package evolution.app.codec.config

import evolution.app.codec.JsonCodec
import evolution.app.portfolio.dsl.Config
import io.circe.Json

object DrawingJsonCodec extends JsonCodec[Config] {

  override def encode(t: Config): Json =
    Json.fromString(t.serialisedExpr)

  override def decode(r: Json): Option[Config] =
    (for {
      serialized <- r.asString.toRight("Unable to parse Json")
      _ = println(s"Parsing inside Json Codec: $serialized")
      (config, state) = Config.from(serialized)
      _ = state.message.foreach(println)
    } yield config).toOption
}
