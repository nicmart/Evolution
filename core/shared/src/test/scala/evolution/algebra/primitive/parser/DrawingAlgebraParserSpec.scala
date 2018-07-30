package evolution.algebra.primitive.parser

import evolution.geometry.Point
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
  "A drawingAlgebraParser should parse" - {
    "double drawings" - {
      "empty" in {
        val serializedExpression = "empty"
        unsafeParse(serializedExpression, container.parser[Drawing[Double]]) shouldBe Empty[Double]()
      }

      "cons" in {
        val serializedExpression = "cons(1, cons(2, empty))"
        val expected = Cons(DoubleScalar(1), Cons(DoubleScalar(2), Empty()))
        unsafeParse(serializedExpression, container.parser[Drawing[Double]]) shouldBe expected
      }

      "recursive" in {
        val serializedExpression = "fix(x -> cons(1, $x))"
        val expected = FixF(LambdaF("x", Cons(DoubleScalar(1), Var0F[Double]())))
        unsafeParse(serializedExpression, container.parser[Drawing[Double]]) shouldBe expected
      }

      "mapCons" in {
        val serializedExpression = "mapCons(empty, h -> t -> cons(1, $t))"
        val expected = FixF(LambdaF("x", Cons(DoubleScalar(1), Var0F[Double]())))
        unsafeParse(serializedExpression, container.parser[Drawing[Double]]) shouldBe expected
      }

      // Problem here, where we need a lambda S[Double] => F[Double] => F[Double],
      // but currently a binding algebra stays inside the same type family
      "mapCons with mixed lambdas" ignore {
        val serializedExpression = "mapCons(empty, h -> t -> cons($h, $t))"
        val expected = FixF(LambdaF("x", Cons(DoubleScalar(1), Var0F[Double]())))
        unsafeParse(serializedExpression, container.parser[Drawing[Double]]) shouldBe expected
      }
    }

    "point drawings" - {
      "empty" in {
        val serializedExpression = "empty"
        unsafeParse(serializedExpression, container.parser[Drawing[Point]]) shouldBe Empty[Point]()
      }

      "cons" in {
        val serializedExpression = "cons(point(0, 0), cons(point(1, 2), empty))"
        val expected = Cons(PointScalar(Point(0, 0)), Cons(PointScalar(Point(1, 2)), Empty()))
        unsafeParse(serializedExpression, container.parser[Drawing[Point]]) shouldBe expected
      }

      "recursive" in {
        val serializedExpression = "fix(x -> cons(point(0, 0), $x))"
        val expected = FixF(LambdaF("x", Cons(PointScalar(Point(0, 0)), Var0F[Point]())))
        unsafeParse(serializedExpression, container.parser[Drawing[Point]]) shouldBe expected
      }
    }
  }

  sealed trait Scalar[A]
  object Scalar {
    case class DoubleScalar(d: Double) extends Scalar[Double]
    case class PointScalar(p: Point) extends Scalar[Point]
    case class Var0S[A]() extends Scalar[A]
    case class ShiftS[A](expr: Scalar[A]) extends Scalar[A]
    case class LetS[A, B](name: String, value: Scalar[A], body: Scalar[B]) extends Scalar[B]
    case class LambdaS[A, B](name: String, expr: Scalar[B]) extends Scalar[B]
    case class FixS[A](expr: Scalar[A]) extends Scalar[A]
  }

  sealed trait Drawing[A]
  object Drawing {
    case class Empty[A]() extends Drawing[A]
    case class Cons[A](head: Scalar[A], tail: Drawing[A]) extends Drawing[A]
    case class MapEmpty[A](eva: Drawing[A], eva2: Drawing[A]) extends Drawing[A]
    case class MapCons[A, B](eva: Drawing[A], f: Drawing[B]) extends Drawing[B]
    case class Var0F[A]() extends Drawing[A]
    case class ShiftF[A](expr: Drawing[A]) extends Drawing[A]
    case class LetF[A, B](name: String, value: Drawing[A], body: Drawing[B]) extends Drawing[B]
    case class LambdaF[A, B](name: String, expr: Drawing[B]) extends Drawing[B]
    case class FixF[A](expr: Drawing[A]) extends Drawing[A]
  }

  object TestInterpreter extends DrawingAlgebra[Scalar, Drawing] {
    override val drawing: CoreDrawingAlgebra[Scalar, Drawing] = new CoreDrawingAlgebra[Scalar, Drawing] {
      override def empty[A]: Drawing[A] = Drawing.Empty()
      override def cons[A](head: Scalar[A], tail: Drawing[A]): Drawing[A] = Drawing.Cons(head, tail)
      override def mapEmpty[A](eva: Drawing[A])(eva2: Drawing[A]): Drawing[A] = Drawing.MapEmpty(eva, eva2)
      override def mapCons[A, B](eva: Drawing[A])(f: Drawing[B]): Drawing[B] = Drawing.MapCons(eva, f)
    }
    override val scalar: ScalarAlgebra[Scalar] = new ScalarAlgebra[Scalar] {
      override def double(d: Double): DrawingAlgebraParserSpec.this.Scalar[Double] = Scalar.DoubleScalar(d)
      override def point(p: Point): DrawingAlgebraParserSpec.this.Scalar[Point] = Scalar.PointScalar(p)
    }
    override val bindS: BindingAlgebra[Scalar] = new BindingAlgebra[Scalar] {
      override def var0[A]: Scalar[A] = Scalar.Var0S()
      override def shift[A](expr: Scalar[A]): Scalar[A] = Scalar.ShiftS(expr)
      override def let[A, B](name: String, value: Scalar[A])(expr: Scalar[B]): Scalar[B] =
        Scalar.LetS(name, value, expr)
      override def lambda[A, B](name: String, expr: Scalar[B]): Scalar[B] = Scalar.LambdaS(name, expr)
      override def fix[A](expr: Scalar[A]): Scalar[A] = Scalar.FixS(expr)
    }
    override val bindF: BindingAlgebra[Drawing] = new BindingAlgebra[Drawing] {
      override def var0[A]: Drawing[A] = Drawing.Var0F()
      override def shift[A](expr: Drawing[A]): Drawing[A] = Drawing.ShiftF(expr)
      override def let[A, B](name: String, value: Drawing[A])(expr: Drawing[B]): Drawing[B] =
        Drawing.LetF(name, value, expr)
      override def lambda[A, B](name: String, expr: Drawing[B]): Drawing[B] = Drawing.LambdaF(name, expr)
      override def fix[A](expr: Drawing[A]): Drawing[A] = Drawing.FixF(expr)
    }
  }

  lazy val parser = new DrawingAlgebraParser[Scalar, Drawing](TestInterpreter)
  lazy val container: DrawingAlgebraParserContainer[Scalar, Drawing] =
    parser.buildContainer(DrawingAlgebraParserContainer.empty[Scalar, Drawing])
}
