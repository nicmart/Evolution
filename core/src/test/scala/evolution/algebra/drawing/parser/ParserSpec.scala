package evolution.algebra.drawing.parser

import evolution.drawing.algebra.Drawing
import evolution.drawing.algebra.interpreter.Serializer
import evolution.drawing.algebra.parser.DrawingParser.DoubleDrawingParser
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks
import evolution.drawing.algebra.interpreter.Builder._
import evolution.drawing.algebra.parser.DrawingParser
import evolution.geometry.Point
import fastparse.all

class ParserSpec
  extends WordSpec
    with Matchers
    with PropertyChecks {

  "A Parser" should {
    "parse a rnd expression" in {
      assertParse("rnd(0.1,1)", rnd(0.1, 1))
      assertParse("rnd(.1,1)", rnd(0.1, 1))
      assertParse("rnd(.1,1.001)", rnd(0.1, 1.001))
    }
    "parse a const expression" in {
      assertParse("const(0.1)", const(0.1))
      assertParse("const(.1)", const(0.1))
      assertParse(".123", const(0.123))
    }
    "parse an integrateDouble expression" in {
      assertParse("integrateDouble(.1,.1)", integrateDouble(.1, const(0.1)))
      assertParse(
        "integrateDouble(.1,integrateDouble(0,const(.1)))",
        integrateDouble(.1, integrateDouble(0, const(0.1)))
      )
    }
    "parse a deriveDouble expression" in {
      assertParse("deriveDouble(1)", deriveDouble(const(1)))
    }
    "parse an integratePoint expression" in {
      assertParse("integratePoint(point(0,0),point(1,1))", integratePoint(Point.zero, cartesian(const(1), const(1))))
    }
    "parse a derivePoint expression" in {
      assertParse("derivePoint(point(1,1))", derivePoint(cartesian(const(1), const(1))))
    }
    "parse an cartesian expression" in {
      assertParse("cartesian(.1,.1)", cartesian(const(.1), const(0.1)))
      assertParse(
        "cartesian(.1,integrateDouble(0,const(.1)))",
        cartesian(const(.1), integrateDouble(0, const(0.1)))
      )
    }
    "parse a polar expression" in {
      assertParse("polar(.1,.1)", polar(const(.1), const(0.1)))
      assertParse(
        "polar(.1,integrateDouble(0,const(.1)))",
        polar(const(.1), integrateDouble(0, const(0.1)))
      )
    }
  }

  def assertParse[T](serializedDrawing: String, expected: Drawing[T])(implicit parser: DrawingParser[T]) = {
    val actual = parser.parse(serializedDrawing)
    actual.map(_.run(Serializer)) shouldBe Right(expected.run(Serializer))
  }
}
