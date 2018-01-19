package evolution.app.codec.config

import evolution.app.codec.JsonCodec
import evolution.drawing.algebra.{Drawing, DrawingAlgebra}
import evolution.drawing.algebra.parser.DrawingParser
import io.circe.Json
import DrawingJsonCodec._
import evolution.drawing.algebra.interpreter.CtxString

class DrawingJsonCodec[T: DrawingAlgebra.Type](
  parser: DrawingParser[T],
  serializer: DrawingAlgebra[CtxString]
) extends JsonCodec[Drawing[Unit, T]]
{
  override def encode(t: Drawing[Unit, T]): Json =  Json.fromString(t.run[CtxString](serializer)(Nil))
  override def decode(r: Json): Option[Drawing[Unit, T]] =
    for {
      serialized <- r.asString
      drawing <- {
        val result = parser.parse(serialized).toOption
        println(serialized)
        println(result)
        result
      }
    } yield drawing
}

object DrawingJsonCodec {
  type ConstString[+A] = String
}
