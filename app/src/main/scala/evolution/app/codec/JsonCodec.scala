package evolution.app.codec

import io.circe.*

object JsonCodec:
  given [T](using encoder: Encoder[T], decoder: Decoder[T]): JsonCodec[T] =
    evolution.app.codec.Codec.instance(
      t => encoder(t),
      json => decoder.decodeJson(json).toOption
    )

  def toCirceEncoder[T](codec: JsonCodec[T]): Encoder[T] =
    a => codec.encode(a)

  def toCirceDecoder[T](codec: JsonCodec[T]): Decoder[T] =
    cursor => codec.decode(cursor.value).toRight(DecodingFailure("Undefined decoding error", Nil))

  object circeImplicits:
    given [T](using codec: JsonCodec[T]): Encoder[T] = toCirceEncoder(codec)
    given [T](using codec: JsonCodec[T]): Decoder[T] = toCirceDecoder(codec)

  /** Summoner method
    */
  def apply[T](using codec: JsonCodec[T]): JsonCodec[T] = codec
