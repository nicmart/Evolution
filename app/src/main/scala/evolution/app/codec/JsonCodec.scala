package evolution.app.codec

import io.circe._

object JsonCodec {

  implicit def fromCirce[T](implicit encoder: Encoder[T], decoder: Decoder[T]): JsonCodec[T] =
    evolution.app.codec.Codec.instance(
      t => encoder(t),
      json => decoder.decodeJson(json).toOption
    )

  def toCirceEncoder[T](codec: JsonCodec[T]): Encoder[T] =
    new Encoder[T] {
      override def apply(a: T): Json = codec.encode(a)
    }

  def toCirceDecoder[T](codec: JsonCodec[T]): Decoder[T] =
    new Decoder[T] {
      override def apply(c: HCursor): Either[DecodingFailure, T] =
        codec.decode(c.value).toRight(DecodingFailure("Undefined decoding error", Nil))
    }

  object circeImplicits {
    implicit def encoder[T](implicit codec: JsonCodec[T]): Encoder[T] =
      toCirceEncoder(codec)
    implicit def decoder[T](implicit codec: JsonCodec[T]): Decoder[T] =
      toCirceDecoder(codec)
  }

  /**
   * Summoner method
   */
  def apply[T](implicit codec: JsonCodec[T]): JsonCodec[T] = codec
}
