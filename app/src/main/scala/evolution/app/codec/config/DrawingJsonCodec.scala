package evolution.app.codec.config

import evolution.app.codec.JsonCodec
import evolution.drawing.algebra.{Drawing, DrawingAlgebra}
import evolution.drawing.algebra.parser.DrawingParser
import io.circe.Json
import DrawingJsonCodec._

class DrawingJsonCodec[T: DrawingAlgebra.Type](
  parser: DrawingParser[T],
  serializer: DrawingAlgebra[ConstString]
) extends JsonCodec[Drawing[T]]
{
  override def encode(t: Drawing[T]): Json =  Json.fromString(t.run[ConstString](serializer))
  override def decode(r: Json): Option[Drawing[T]] =
    for {
      serialized <- r.asString
      drawing <- parser.parse(serialized).toOption
    } yield drawing
}

object DrawingJsonCodec {
  type ConstString[+A] = String
}
