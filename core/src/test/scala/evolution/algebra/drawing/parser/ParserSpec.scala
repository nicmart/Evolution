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
      assertParse("rnd(-.1,1.001)", rnd(-0.1, 1.001))
    }
    "ignore whitespaces" in {
      assertParse("rnd(0.1, 1)", rnd(0.1, 1))
      assertParse("rnd(\n.1,\n1)", rnd(0.1, 1))
    }
    "parse a const expression" in {
      assertParse("0.1", const(0.1))
      assertParse(".1", const(0.1))
      assertParse(".123", const(0.123))
    }
    "parse an integrate of a double expression" in {
      assertParse("integrate(.1,.1)", integrate(.1, const(0.1)))
      assertParse(
        "integrate(.1,integrate(0,.1))",
        integrate(.1, integrate(0.0, const(0.1)))
      )
    }
    "parse a derive Double expression" in {
      assertParse("derive(1)", derive(const(1.0)))
    }
    "parse an integrate Point expression" in {
      assertParse("integrate(point(0,0),point(1,1))", integrate(Point.zero, const(Point(1, 1))))
    }
    "parse a derive Point expression" in {
      assertParse("derive(point(1,1))", derive(const(Point(1, 1))))
    }
    "parse a point expression" in {
      assertParse("point(.1,.1)", point(const(.1), const(0.1)))
      assertParse(
        "point(.1,integrate(0,.1))",
        point(const(.1), integrate(0, const(0.1)))
      )
    }
    "parse a polar expression" in {
      assertParse("polar(.1,.1)", polar(const(.1), const(0.1)))
      assertParse(
        "polar(.1,integrate(0,.1))",
        polar(const(.1), integrate(0, const(0.1)))
      )
    }
  }

  def assertParse[T](serializedDrawing: String, expected: Drawing[T])(implicit parser: DrawingParser[T]) = {
    val actual = parser.parse(serializedDrawing)
    actual.map(_.run(Serializer)) shouldBe Right(expected.run(Serializer))
  }
}
