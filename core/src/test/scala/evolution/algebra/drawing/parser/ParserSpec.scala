package evolution.algebra.drawing.parser

import evolution.drawing.algebra._
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

  type OpenDoubleEnv[F[_, _]] = (F[Double, Empty[F]], Empty[F])

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
      assertParse(
        "\n\nlet(x\n  ,\n  .1  \n , $x   \n)",
        let0("x", const(.1))(var0[Empty, Double])
      )
      assertSuccessfulParse(
        "\n\nlet\n(\nx\n,\n.1\n,\n let \n ( \n y\n,\n.1\n,\n$y\n)\n)\n"
      )
      assertParse(
        "point(1,rnd(1,1\n)\n)\n",
        point(const(1.0), rnd(1, 1))
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
      assertParse("let(x,1,$x)", let[Empty, Double, Double]("x", const(1.0))(var0))
      assertDoubleParse[Point]("let(x,1.0,point($x,$x))")
      assertDoubleParse[Point]("let(x,1.0,let(y,1.0,point($x,$y)))")
    }
  }

  type Env0[F[_, _]] = Empty[F]
  type Env1[F[_, _], In] = (F[Env0[F], In], Env0[F])
  type Env2[F[_, _], In1, In2] = (F[Env1[F, In2], In1], Env1[F, In2])
  type EnvS[F[_, _], E[_[_, _]], In] = (F[E[F], In], E[F])

  type Expr0[Out] = DrawingExpr[Empty, Out]
  type Expr1[In, Out] = DrawingExpr[λ[F[_, _] => Env1[F, In]], Out]
  type Expr2[In1, In2, Out] = DrawingExpr[λ[F[_, _] => Env2[F, In1, In2]], Out]

  private def let0[In, Out](varname: String, value: Expr0[In])(expr: Expr1[In, Out]): Expr0[Out] =
    let[Empty, In, Out](varname, value)(expr)

  private def let1[In1, In2, Out](varname: String, value: Expr1[In1, In2])(expr: Expr2[In2, In1, Out]): Expr1[In1, Out] =
    let[Env1[?[_, _], In1], In2, Out](varname, value)(expr)

  def assertParse[T](serializedDrawing: String, expected: DrawingExpr[Empty, T])(implicit parser: DrawingParser[T]) = {
    val actual = parser.parse(serializedDrawing)
    actual.map(_.run(Serializer)(Nil)) shouldBe Right(expected.run(Serializer)(Nil))
  }

  def assertSuccessfulParse[T](serialized: String)(implicit parser: DrawingParser[T]) = {
    val actual = parser.parse(serialized)
    actual.map(_.run(Serializer)(Nil)).isRight shouldBe true
  }

  def assertDoubleParse[T](serializedDrawing: String)(implicit parser: DrawingParser[T]) = {
    val actual = parser.parse(serializedDrawing)
    actual.map(_.run(Serializer)(Nil)) shouldBe Right(serializedDrawing)
  }
}
