package evolution.app.codec

import java.util.Base64

import scala.util.Try

object Base64Codec extends Codec[Array[Byte], String]:

  override def encode(t: Array[Byte]): String =
    Base64.getEncoder.encodeToString(t)

  override def decode(r: String): Option[Array[Byte]] =
    Try(Base64.getDecoder.decode(r)).toOption
