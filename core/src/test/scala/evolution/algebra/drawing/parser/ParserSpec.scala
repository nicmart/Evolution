package evolution.algebra.drawing.parser

import evolution.drawing.algebra._
import evolution.drawing.algebra.interpreter.{Builder, Serializer}
import evolution.drawing.algebra.parser.DrawingParser.DoubleDrawingParser
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks
import evolution.drawing.algebra.parser.{DrawingParser}
import evolution.geometry.Point

class ParserSpec
  extends WordSpec
    with Matchers
    with PropertyChecks {

  import Builder.start._

  "A Parser" should {
    "parse a rnd expression" in {
      assertParse("rnd(0.1, 1)", rnd(const(0.1), const(1)))
      assertParse("rnd(.1, 1)", rnd(const(0.1), const(1)))
      assertParse("rnd(.1, 1.001)", rnd(const(0.1), const(1.001)))
      assertParse("rnd(-.1, 1.001)", rnd(const(-0.1), const(1.001)))
    }
    "parse an add expression" in {
      assertParse("add(0.1, 1.0)", add(const(0.1), const(1.0)))
    }
    "parse an add expression in a point" in {
      assertParse("point(0, add(0.1, 1.0))", point(const(0), add(const(0.1), const(1.0))))
    }
    "ignore whitespaces" in {
      assertParse("rnd(0.1, 1)", rnd(const(0.1), const(1)))
      assertParse("rnd(\n.1, \n1)", rnd(const(0.1), const(1)))
      assertParse(
        "\n\nlet(x\n  ,\n  .1  \n , $x   \n)",
        let("x", const(.1))(_ => var0)
      )
      assertParse[Double](
        "\n\nlet\n(\nx\n,\n.1\n,\n let \n ( \n y\n,\n.1\n,\n$y\n)\n)\n",
        let("x", const(.1))(b1 => b1.let("y", b1.const(.1))(_ => b1.var0))
      )
      assertParse(
        "point(1,rnd(1,1\n)\n)\n",
        point(const(1.0), rnd(const(1), const(1)))
      )
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
    "parse let binding" in {
      assertParse("let(x,1,$x)", let("x", const(1.0))(_ => var0))
      assertDoubleParse[Point]("let(x, 1.0, point($x, $x))")
      assertDoubleParse[Point]("let(x, 1.0, let(y, 1.0, point($x, $y)))")
    }

    "parse let binding with infix notation" in {
      assertParse("x = 1 $x", let("x", const(1.0))(_ => var0))
      assertParse(
        " x = 1 y = point($x, $x) $y ",
        let("x", const(1.0))(b1 => b1.let("y", b1.point(var0, var0))(_ => b1.var0))
      )
    }

    "parse let binding when a variable is the prefix of another" in {
      assertParse("x = 1 xx = 2 $xx", let("x", const(1.0))(b1 => b1.let("xx", b1.const(2.0))(b2 => b1.var0)))
      assertParse("xx = 1 x = 2 $xx", let("xx", const(1.0))(b1 => b1.let("x", b1.const(2.0))(b2 => b1.shift(var0))))
    }

    "parse an inverse with infix notation" in {
      assertParse("x = 1 -$x", let("x", const(1.0))(b1 => b1.inverse(var0)))
    }

    "parse a choose expression" in {
      assertParse(
        "choose(0.5, rnd(-1, 1), 0)",
        choose(const(0.5), rnd(const(-1.0), const(1.0)), const(0.0))
      )
    }

    "do not parse partial drawings" in {
      assertNotParse[Point]("point(1,1) point(1,1)")
    }
  }

  private def assertParse[T](serializedDrawing: String, expected: DrawingExpr[Empty, T])(implicit parser: DrawingParser[T]) = {
    val actual = parser.parse(serializedDrawing)
    actual.map(_.run(Serializer)(Nil)) shouldBe Right(expected.run(Serializer)(Nil))
  }

  private def assertNotParse[T](serializedDrawing: String)(implicit parser: DrawingParser[T]) = {
    val actual = parser.parse(serializedDrawing)
    actual.isLeft shouldBe true
  }

  private def assertDoubleParse[T](serializedDrawing: String)(implicit parser: DrawingParser[T]) = {
    val actual = parser.parse(serializedDrawing)
    actual.map(_.run(Serializer)(Nil)) shouldBe Right(serializedDrawing)
  }
}
