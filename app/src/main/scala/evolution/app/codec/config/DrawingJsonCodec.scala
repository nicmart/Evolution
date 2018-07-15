package evolution.app.codec.config

import evolution.app.codec.JsonCodec
import evolution.drawing.algebra.{DrawingAlgebra, DrawingExpr, Type}
import evolution.drawing.algebra.parser.DrawingParser
import io.circe.Json
import DrawingJsonCodec._
import evolution.drawing.algebra.interpreter.CtxString

class DrawingJsonCodec[T: Type](parser: DrawingParser[T], serializer: DrawingAlgebra[CtxString])
    extends JsonCodec[DrawingExpr[T]] {
  override def encode(t: DrawingExpr[T]): Json =
    Json.fromString(t.run[CtxString](serializer)(Nil))
  override def decode(r: Json): Option[DrawingExpr[T]] =
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
