package evolution.compiler.materializing

import evolution.compiler.LanguageSpec
import _root_.evolution.compiler.phases.materializing.MaterializeJsCode
import _root_.evolution.data.Expr
import scala.scalajs.js.Function
import scala.scalajs.js
import evolution.geometry.Point
import org.scalatest.Inspectors
import evolution.typeclass.Semigroupoid

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

    "materialize constant evolutions" in {
      val jsCode = MaterializeJsCode.materialize(Expr.Constant(Expr.Dbl(1.1)))
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      result.iterator.take(10).toList shouldBe List.fill(10)(1.1)
    }

    "materialize uniform evolutions" in {
      val jsCode = MaterializeJsCode.materialize(Expr.Uniform(Expr.Dbl(0), Expr.Dbl(1)))
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      val first100 = result.iterator.take(100).toList
      Inspectors.forAll(first100) { d =>
        d should (be >= 0.0 and be <= 1.0)
      }
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

    "mapped evolution" in {
      val expr = Expr.Map(
        Expr.Constant(Expr.Dbl(1)),
        Expr.Lambda("x", Expr.Add(Expr.Var("x"), Expr.Var("x"), Semigroupoid.Additive.dblDblDbl))
      )
      val jsCode = MaterializeJsCode.materialize(expr)
      val result = evaluate(jsCode).asInstanceOf[js.Iterable[Double]]
      val first100 = result.iterator.take(100).toList
      first100 shouldBe List.fill(100)(2)
    }
  }

  private def evaluate(expr: String): Any = {

    val f = new Function(s"return $expr;")
    f.call((), ())
  }
}
