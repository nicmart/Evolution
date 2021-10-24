package evolution.app.codec

import io.circe.Json
import io.circe.parser.parse

object JsonStringCodec extends Codec[Json, String]:
  override def encode(t: Json): String =
    t.noSpaces

  override def decode(r: String): Option[Json] =
    parse(r).toOption
