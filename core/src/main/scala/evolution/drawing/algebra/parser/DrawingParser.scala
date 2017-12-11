package evolution.drawing.algebra.parser

import evolution.drawing.algebra.interpreter.Builder._
import evolution.drawing.algebra.{Drawing, DrawingAlgebra}

trait DrawingParser[+A] {
  def parse(s: String): Either[String, Drawing[A]]
  // def parse[F[+_]](alg: DrawingAlgebra[F])(s: String): Either[String, F[A]]
}

object DrawingParser {
  type Error = String

  object DoubleDrawingParser extends DrawingParser[Double] {
    val rndRegexp = """rnd\((\d+(?:\.\d+)?),(\d+(?:\.\d+)?)\)""".r
    override def parse(s: String): Either[String, Drawing[Double]] = s match {
      case rndRegexp(d1, d2) =>  Right(rnd(d1.toDouble, d2.toDouble))
      case _ => Left("Error")
    }
  }
}
