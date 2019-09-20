package evolution.compiler.materializing.evaluation

import evolution.compiler.LanguageSpec
import evolution.compiler.impl.jsmaterialization.MaterializeJsCode
import evolution.compiler.expression.Expr
import scala.scalajs.js.Function
import scala.scalajs.js
import evolution.geometry.Point
import org.scalatest.Inspectors
import evolution.compiler.expression.typeclass._

class MaterializeJsCodeSpec extends LanguageSpec {
  "Materializing expressions" - {
    "materialize doubles" in {
      val jsCode = MaterializeJsCode.materialize(Expr.Dbl(1.23))
      val result = evaluate(jsCode)
      result shouldBe 1.23
    }

    "materialize points" in {
      val jsCode = MaterializeJsCode.materialize(Expr.Pnt(Expr.Dbl(1), Expr.Dbl(2)))
      val result = evaluate(jsCode).asInstanceOf[Point]
      result shouldBe Point(1, 2)
    }

    "materialize points in polar coordinates" in {
      val jsCode = MaterializeJsCode.materialize(Expr.Polar(Expr.Dbl(1), Expr.Dbl(0)))
      val result = evaluate(jsCode).asInstanceOf[Point]
      result shouldBe Point(1, 0)
    }

    "materialize x projections" in {
      val expr = Expr.X(Expr.Pnt(Expr.Dbl(1), Expr.Dbl(2)))
      val jsCode = MaterializeJsCode.materialize(expr)
      val result = evaluate(jsCode).asInstanceOf[Double]
      result shouldBe 1
    }

    "materialize y projections" in {
      val expr = Expr.Y(Expr.Pnt(Expr.Dbl(1), Expr.Dbl(2)))
      val jsCode = MaterializeJsCode.materialize(expr)
      val result = evaluate(jsCode).asInstanceOf[Double]
      result shouldBe 2
    }

    "materialize smoothstep" - {
      "on the left of the interval should be 0" in {
        val expr = Expr.SmoothStep(Expr.Dbl(0), Expr.Dbl(10), Expr.Dbl(-1))
        val jsCode = MaterializeJsCode.materialize(expr)
        val result = evaluate(jsCode).asInstanceOf[Double]
        result shouldBe 0
      }

      "on the right of the interval should be 1" in {
        val expr = Expr.SmoothStep(Expr.Dbl(0), Expr.Dbl(10), Expr.Dbl(11))
        val jsCode = MaterializeJsCode.materialize(expr)
        val result = evaluate(jsCode).asInstanceOf[Double]
        result shouldBe 1
      }

      "on the interval should be between the 0 and 1" in {
        val expr = Expr.SmoothStep(Expr.Dbl(0), Expr.Dbl(10), Expr.Dbl(5))
        val jsCode = MaterializeJsCode.materialize(expr)
        val result = evaluate(jsCode).asInstanceOf[Double]
        result should (be >= 0.0 and be <= 1.0)
      }
    }

    "materialize cons" in {
      val jsCode = MaterializeJsCode.materialize(Expr.Cons(Expr.Dbl(1), Expr.Constant(Expr.Dbl(2))))
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      result.iterator.take(10).toList shouldBe 1 :: List.fill(9)(2)
    }

    "materialize constant evolutions" in {
      val jsCode = MaterializeJsCode.materialize(Expr.Constant(Expr.Dbl(1.1)))
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      result.iterator.take(10).toList shouldBe List.fill(10)(1.1)
    }

    "materialize lifted points" in {
      val uniform = Expr.Uniform(Expr.Dbl(0), Expr.Dbl(1))
      val jsCode = MaterializeJsCode.materialize(Expr.LiftedPnt(uniform, uniform))
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Point]]
      val first100 = result.iterator.take(100).toList
      Inspectors.forAll(first100) { d =>
        d.x should (be >= 0.0 and be <= 1.0)
        d.y should (be >= 0.0 and be <= 1.0)
      }
    }

    "materialize lifted polars" in {
      val r = Expr.Constant(Expr.Dbl(1))
      val alpha = Expr.Constant(Expr.Dbl(0))
      val jsCode = MaterializeJsCode.materialize(Expr.LiftedPolar(r, alpha))
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Point]]
      val first100 = result.iterator.take(100).toList
      Inspectors.forAll(first100) { p =>
        p shouldBe Point.polar(1, 0)
      }
    }

    "materialize mapped evolution" in {
      val expr = Expr.Map(
        Expr.Constant(Expr.Dbl(1)),
        Expr.Lambda("x", Expr.Add(Expr.Var("x"), Expr.Var("x"), Additive.DoubleDoubleDouble))
      )
      val jsCode = MaterializeJsCode.materialize(expr)
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      val first100 = result.iterator.take(100).toList
      first100 shouldBe List.fill(100)(2)
    }

    "materialize flatMapped evolutions" in {
      val expr = Expr.FlatMap(
        Expr.Constant(Expr.Dbl(1)),
        Expr.Lambda("x", Expr.Cons(Expr.Dbl(2), Expr.Cons(Expr.Var("x"), Expr.Empty())))
      )
      val jsCode = MaterializeJsCode.materialize(expr)
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      result.iterator.take(8).toList shouldBe List.fill(4)(List(2, 1)).flatten
    }

    "materialize flatten evolutions" in {
      val expr = Expr.Flatten(
        Expr.Constant(Expr.Constant(Expr.Dbl(1)))
      )
      val jsCode = MaterializeJsCode.materialize(expr)
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      result.iterator.take(10).toList shouldBe List.fill(10)(1)
    }

    "materialize integrations" in {
      val expr = Expr.Integrate(
        Expr.Dbl(0),
        Expr.Constant(Expr.Dbl(1)),
        Additive.DoubleDoubleDouble
      )

      val jsCode = MaterializeJsCode.materialize(expr)
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      val first100 = result.iterator.take(100).toList
      first100 shouldBe (0 to 99).toList
    }

    "materialize solve1" in {
      val expr = Expr.Solve1(
        Expr.Constant(Expr.Lambda("x", Expr.Var("x"))),
        Expr.Dbl(1),
        Additive.DoubleDoubleDouble
      )

      val jsCode = MaterializeJsCode.materialize(expr)
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      result.iterator.take(4).toList shouldBe List(1, 2, 4, 8)
    }

    "materialize solve2" in {
      val expr = Expr.Solve2(
        Expr.Constant(Expr.Lambda("x", Expr.Lambda("y", Expr.Var("y")))),
        Expr.Dbl(0),
        Expr.Dbl(1),
        Additive.DoubleDoubleDouble
      )

      val jsCode = MaterializeJsCode.materialize(expr)
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      result.iterator.take(4).toList shouldBe List(0, 2, 6, 14)
    }

    "materialize lists" in {
      val jsCode = MaterializeJsCode.materialize(Expr.Lst(List(Expr.Dbl(0), Expr.Dbl(1), Expr.Dbl(2))))
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      result.iterator.take(100).toList shouldBe List(0, 1, 2)
    }

    "materialize lambdas with more than one argument" in {
      val f = Expr.Lambda("x", Expr.Lambda("y", Expr.Pnt(Expr.Var("x"), Expr.Var("y"))))
      val jsCode = MaterializeJsCode.materialize(f)
      val result = evaluate(jsCode).asInstanceOf[js.Function1[Double, js.Function1[Double, Point]]]
      result(1)(2) shouldBe Point(1, 2)
    }

    "materialize zipWiths" in {
      val c1 = Expr.Constant(Expr.Dbl(1))
      val c2 = Expr.Constant(Expr.Dbl(2))
      val f = Expr.Lambda("x", Expr.Lambda("y", Expr.Pnt(Expr.Var("x"), Expr.Var("y"))))
      val jsCode = MaterializeJsCode.materialize(Expr.ZipWith(c1, c2, f))
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Point]]
      val first100 = result.iterator.take(100).toList
      first100 shouldBe List.fill(100)(Point(1, 2))
    }

    "materialize takes" in {
      val expr = Expr.Take(Expr.Integer(10), Expr.Constant(Expr.Dbl(1)))
      val jsCode = MaterializeJsCode.materialize(expr)
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Point]]
      result.iterator.take(100).toList shouldBe List.fill(10)(1)
    }

    "materialize takeWhile" in {
      val list = Expr.Cons(Expr.Dbl(1), Expr.Cons(Expr.Dbl(-1), Expr.Cons(Expr.Dbl(1), Expr.Empty())))
      val predicate = Expr.Lambda("x", Expr.GreaterThan(Expr.Var("x"), Expr.Dbl(0), Comparable.Double))
      val expr = Expr.TakeWhile(list, predicate)
      val jsCode = MaterializeJsCode.materialize(expr)
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      result.iterator.take(100).toList shouldBe List(1)
    }

    "materialize concats" in {
      val ev1 = Expr.Cons(Expr.Dbl(1), Expr.Empty())
      val ev2 = Expr.Cons(Expr.Dbl(2), Expr.Empty())
      val expr = Expr.Concat(ev1, ev2)
      val jsCode = MaterializeJsCode.materialize(expr)
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      result.iterator.take(100).toList shouldBe List(1, 2)
    }

    "materialize withFirst" in {
      val expr = Expr.WithFirst(
        Expr.Constant(Expr.Dbl(1)),
        Expr.Lambda("x", Expr.Cons(Expr.Dbl(2), Expr.Cons(Expr.Var("x"), Expr.Empty())))
      )
      val jsCode = MaterializeJsCode.materialize(expr)
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      result.iterator.take(100).toList shouldBe List(2, 1)
    }

    "materialize ranges" in {
      val expr = Expr.Range(Expr.Dbl(0), Expr.Dbl(10.5), Expr.Dbl(1))
      val jsCode = MaterializeJsCode.materialize(expr)
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Point]]
      result.iterator.take(100).toList shouldBe List.range(0, 11)
    }

    "materialize uniform evolutions" in {
      val jsCode = MaterializeJsCode.materialize(Expr.Uniform(Expr.Dbl(0), Expr.Dbl(1)))
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      val first100 = result.iterator.take(100).toList
      Inspectors.forAll(first100) { d =>
        d should (be >= 0.0 and be <= 1.0)
      }
    }

    "materialize uniform choices" in {
      val jsCode = MaterializeJsCode.materialize(Expr.UniformChoice(Expr.Lst(List(Expr.Dbl(0), Expr.Dbl(1)))))
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      val first100 = result.iterator.take(100).toList
      first100 should have length (100)
      Inspectors.forAll(first100) { d =>
        Set(0, 1) should contain(d)
      }
    }

    "materialize uniformDiscrete" in {
      val jsCode = MaterializeJsCode.materialize(Expr.UniformDiscrete(Expr.Dbl(2), Expr.Dbl(10.5), Expr.Dbl(2)))
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      val first100 = result.iterator.take(100).toList
      val choices = List(2, 4, 6, 8, 10)
      Inspectors.forAll(first100) { d =>
        choices should contain(d)
      }
    }

    "materialize uniformFrom" in {
      val jsCode = MaterializeJsCode.materialize(
        Expr.UniformFrom(Expr.Integer(2), Expr.Range(Expr.Dbl(1), Expr.Dbl(10), Expr.Dbl(1)))
      )
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      val first100 = result.iterator.take(100).toList
      val choices = List(1, 2)
      Inspectors.forAll(first100) { d =>
        choices should contain(d)
      }
    }

  }

  private def evaluate(expr: String): Any = {

    val f = new Function(s"return $expr;")
    f.call((), ())
  }
}
