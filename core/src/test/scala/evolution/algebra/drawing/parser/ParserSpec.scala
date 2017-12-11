package evolution.algebra.drawing.parser

import evolution.drawing.algebra.interpreter.Serializer
import evolution.drawing.algebra.parser.DrawingParser.DoubleDrawingParser
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks

class ParserSpec
  extends WordSpec
    with Matchers
    with PropertyChecks {

  "A Parser" should {
    "parse a rnd expression" in {
      val serialized = "rnd(0.01,1.01)"

      DoubleDrawingParser.parse(serialized).map(_.run(Serializer)) shouldBe Right(serialized)
    }
  }

}
