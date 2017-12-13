package evolution.algebra.drawing.parser

import evolution.drawing.algebra.Drawing
import evolution.drawing.algebra.interpreter.Serializer
import evolution.drawing.algebra.parser.DrawingParser.DoubleDrawingParser
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks
import evolution.drawing.algebra.interpreter.Builder._

class ParserSpec
  extends WordSpec
    with Matchers
    with PropertyChecks {

  "A Parser" should {
    "parse a rnd expression" in {
      assertSuccessfulParse("rnd(0.1,1)", rnd(0.1, 1))
      assertSuccessfulParse("rnd(.1,1)", rnd(0.1, 1))
      assertSuccessfulParse("rnd(.1,1.001)", rnd(0.1, 1.001))
    }
    "parse a const expression" in {
      assertSuccessfulParse("const(0.1)", const(0.1))
      assertSuccessfulParse("const(.1)", const(0.1))
      assertSuccessfulParse(".123", const(0.123))
    }
    "parse an integrateDouble expression" in {
      assertSuccessfulParse("integrateDouble(.1,.1)", integrateDouble(.1, const(0.1)))
      assertSuccessfulParse(
        "integrateDouble(.1,integrateDouble(0,const(.1)))",
        integrateDouble(.1, integrateDouble(0, const(0.1)))
      )
    }
  }

  def assertSuccessfulParse(serializedDrawing: String, expected: Drawing[Double]) = {
    val actual = DoubleDrawingParser.parse(serializedDrawing)
    actual.map(_.run(Serializer)) shouldBe Right(expected.run(Serializer))
  }
}
