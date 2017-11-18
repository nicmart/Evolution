package evolution.app.react.component.config

import io.circe.{Decoder, Encoder, Json}

trait ConfigCodec[Config] {
  def encode(config: Config): Json
  def decode(json: Json): Option[Config]
}

object ConfigCodec {
  def instance[C](enc: C => Json, dec: Json => Option[C]): ConfigCodec[C] =
    new ConfigCodec[C] {
      override def encode(config: C): Json = enc(config)
      override def decode(json: Json): Option[C] = dec(json)
    }

  implicit
  def fromCirce[Config](implicit encoder: Encoder[Config], decoder: Decoder[Config]): ConfigCodec[Config] =
    instance(
      config => encoder(config),
      json => decoder.decodeJson(json).toOption
    )

  /**
    * Summoner method
    */
  def apply[Config](implicit codec: ConfigCodec[Config]): ConfigCodec[Config]
    = codec
}
