package evolution.algebra.primitive.parser

import evolution.geometry.Point
import evolution.primitive.algebra.interpreter.Id
import evolution.primitive.algebra.{BindingAlgebra, CoreDrawingAlgebra, DrawingAlgebra, ScalarAlgebra}
import evolution.primitive.algebra.parser.{DrawingAlgebraParser, DrawingAlgebraParserContainer, ParserConfig}
import org.scalatest.{FreeSpec, Matchers}
import evolution.primitive.algebra.parser.PrimitiveParsers._

class DrawingAlgebraParserSpec extends FreeSpec with Matchers with CommonTestParsers {
  import ParserConfig.White._
  import evolution.primitive.algebra.parser.PrimitiveParsers._
  import fastparse.noApi._
  import Drawing._
  import Scalar._
  import Binding._, Drawing._, Scalar._
  "A drawingAlgebraParser should parse" - {
    "double drawings" - {
      "empty" in {
        val serializedExpression = "empty"
        unsafeParse(serializedExpression, container.parser[Binding[Drawing[Double]]]) shouldBe DrawingB(Empty[Double]())
      }

      "cons" in {
        val serializedExpression = "cons(1, cons(2, empty))"
        val expected =
          DrawingB(Cons(ScalarB(DoubleScalar(1)), DrawingB(Cons(ScalarB(DoubleScalar(2)), DrawingB(Empty())))))
        unsafeParse(serializedExpression, container.parser[Binding[Drawing[Double]]]) shouldBe expected
      }

      "recursive" in {
        val serializedExpression = "fix(x -> cons(1, $x))"
        val expected = Fix(Lambda("x", DrawingB(Cons(ScalarB(DoubleScalar(1)), Var0[Drawing[Double]]()))))
        unsafeParse(serializedExpression, container.parser[Binding[Drawing[Double]]]) shouldBe expected
      }

      "mapCons" in {
        val serializedExpression = "mapCons(empty, h -> t -> cons(1, $t))"
        val expected = DrawingB(
          MapCons(
            DrawingB(Empty()),
            Lambda("h", Lambda("t", DrawingB(Cons(ScalarB(DoubleScalar(1.0)), Var0[Drawing[Double]]()))))
          )
        )
        unsafeParse(serializedExpression, container.parser[Binding[Drawing[Double]]]) shouldBe expected
      }

      // Problem here, where we need a lambda S[Double] => F[Double] => F[Double],
      // but currently a binding algebra stays inside the same type family
      "mapCons with mixed lambdas" in {
        val serializedExpression = "mapCons(empty, h -> t -> cons($h, $t))"
        val expected = DrawingB(
          MapCons(
            DrawingB(Empty()),
            Lambda("h", Lambda("t", DrawingB(Cons(Shift(Var0[Scalar[Double]]()), Var0[Drawing[Double]]()))))
          )
        )
        unsafeParse(serializedExpression, container.parser[Binding[Drawing[Double]]]) shouldBe expected
      }
    }

//    "point drawings" - {
//      "empty" in {
//        val serializedExpression = "empty"
//        unsafeParse(serializedExpression, container.parser[Drawing[Point]]) shouldBe Empty[Point]()
//      }
//
//      "cons" in {
//        val serializedExpression = "cons(point(0, 0), cons(point(1, 2), empty))"
//        val expected = Cons(PointScalar(Point(0, 0)), Cons(PointScalar(Point(1, 2)), Empty()))
//        unsafeParse(serializedExpression, container.parser[Drawing[Point]]) shouldBe expected
//      }
//
//      "recursive" in {
//        val serializedExpression = "fix(x -> cons(point(0, 0), $x))"
//        val expected = FixF(LambdaF("x", Cons(PointScalar(Point(0, 0)), Var0F[Point]())))
//        unsafeParse(serializedExpression, container.parser[Drawing[Point]]) shouldBe expected
//      }
//    }
  }

  sealed trait Binding[A]
  object Binding {
    case class Var0[A]() extends Binding[A]
    case class Shift[A](expr: Binding[A]) extends Binding[A]
    case class Let[A, B](name: String, value: Binding[A], body: Binding[B]) extends Binding[B]
    case class Lambda[A, B](name: String, expr: Binding[B]) extends Binding[B]
    case class Fix[A](expr: Binding[A]) extends Binding[A]

    case class ScalarB[A](s: Scalar[A]) extends Binding[Scalar[A]]
    case class DrawingB[A](s: Drawing[A]) extends Binding[Drawing[A]]
  }

  sealed trait Scalar[A]
  object Scalar {
    case class DoubleScalar(d: Double) extends Scalar[Double]
    case class PointScalar(p: Point) extends Scalar[Point]
  }

  type BScalar[T] = Binding[Scalar[T]]
  type BDrawing[T] = Binding[Drawing[T]]

  sealed trait Drawing[A]
  object Drawing {
    case class Empty[A]() extends Drawing[A]
    case class Cons[A](head: Binding[Scalar[A]], tail: Binding[Drawing[A]]) extends Drawing[A]
    case class MapEmpty[A](eva: Binding[Drawing[A]], eva2: Binding[Drawing[A]]) extends Drawing[A]
    case class MapCons[A, B](eva: Binding[Drawing[A]], f: Binding[Drawing[B]]) extends Drawing[B]
  }

  object TestInterpreter extends DrawingAlgebra[Scalar, Drawing, Binding] {
    override val drawing: CoreDrawingAlgebra[BScalar, BDrawing] = new CoreDrawingAlgebra[BScalar, BDrawing] {
      override def empty[A]: BDrawing[A] = DrawingB(Drawing.Empty())
      override def cons[A](head: BScalar[A], tail: BDrawing[A]): BDrawing[A] = DrawingB(Drawing.Cons(head, tail))
      override def mapEmpty[A](eva: BDrawing[A])(eva2: BDrawing[A]): BDrawing[A] = DrawingB(Drawing.MapEmpty(eva, eva2))
      override def mapCons[A, B](eva: BDrawing[A])(f: BDrawing[B]): BDrawing[B] = DrawingB(Drawing.MapCons(eva, f))
    }
    override val scalar: ScalarAlgebra[BScalar] = new ScalarAlgebra[BScalar] {
      override def double(d: Double): BScalar[Double] = ScalarB(Scalar.DoubleScalar(d))
      override def point(p: Point): BScalar[Point] = ScalarB(Scalar.PointScalar(p))
    }
    override val bind: BindingAlgebra[Binding] = new BindingAlgebra[Binding] {
      override def var0[A]: Binding[A] = Binding.Var0()
      override def shift[A](expr: Binding[A]): Binding[A] = Binding.Shift(expr)
      override def let[A, B](name: String, value: Binding[A])(expr: Binding[B]): Binding[B] =
        Binding.Let(name, value, expr)
      override def lambda[A, B](name: String, expr: Binding[B]): Binding[B] = Binding.Lambda(name, expr)
      override def fix[A](expr: Binding[A]): Binding[A] = Binding.Fix(expr)
    }
  }

  lazy val parser = new DrawingAlgebraParser[Scalar, Drawing, Binding](TestInterpreter)
  lazy val container: DrawingAlgebraParserContainer[Scalar, Drawing, Binding] =
    parser.buildContainer(DrawingAlgebraParserContainer.empty[Scalar, Drawing, Binding])
}
