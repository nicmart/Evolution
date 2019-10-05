package evolution.compiler

import evolution.data.EvaluationContext._
import evolution.geometry.Point
import evolution.compiler.expression.Expr
import evolution.compiler.expression.Expr._
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ FreeSpec, Matchers }
import impl.evaluation.EvalMaterializer.materializeExpr
import evolution.materialization.Evolution
import evolution.compiler.expression.typeclass._
import evolution.compiler.impl.evaluation.MaterializeEquality
import evolution.compiler.impl.evaluation.MaterializeComparison

class EvalMaterializeSpec extends FreeSpec with GeneratorDrivenPropertyChecks with Matchers {

  "The materializer" - {
    "should materialize Pnt" in {
      materializeExpr(Pnt(Dbl(0), Dbl(0)))(emptyCtx) shouldBe Point(0, 0)
    }

    "should materialize booleans literals" in forAll(genBooleanLiteral) { literal =>
      materializeExpr(literal)(emptyCtx) shouldBe literal.b
    }

    "should materialize ands" in forAll(genBooleanLiteral, genBooleanLiteral) { (a, b) =>
      materializeExpr(And(a, b))(emptyCtx) shouldBe a.b && b.b
    }

    "should materialize ors" in forAll(genBooleanLiteral, genBooleanLiteral) { (a, b) =>
      materializeExpr(Or(a, b))(emptyCtx) shouldBe a.b || b.b
    }

    "should materialize nots" in forAll(genBooleanLiteral) { a =>
      materializeExpr(Not(a))(emptyCtx) shouldBe !a.b
    }

    "should materialize inRect statements" in {
      materializeExpr(InRect(Pnt(Dbl(0), Dbl(0)), Pnt(Dbl(10), Dbl(10)), Pnt(Dbl(5), Dbl(5))))(emptyCtx) shouldBe true
      materializeExpr(InRect(Pnt(Dbl(0), Dbl(0)), Pnt(Dbl(10), Dbl(10)), Pnt(Dbl(20), Dbl(5))))(emptyCtx) shouldBe false
    }

    "should materialize relational operators" in forAll(
      genRelationOperatorExpectations(Comparable.Double),
      arbitrary[Dbl],
      arbitrary[Dbl]
    ) {
      case ((op, expected), a, b) =>
        materializeExpr(op(a, b))(emptyCtx) shouldBe expected(
          materializeExpr(a)(emptyCtx),
          materializeExpr(b)(emptyCtx)
        )
    }

    "should materialize equality operators" in forAll(
      genEqualityOperatorExpectations(Equable.Double),
      arbitrary[Dbl],
      arbitrary[Dbl]
    ) {
      case ((op, expected), a, b) =>
        materializeExpr(op(a, b))(emptyCtx) shouldBe expected(
          materializeExpr(a)(emptyCtx),
          materializeExpr(b)(emptyCtx)
        )
    }

    "should materialize uniformChoices" in forAll(arbitrary[List[Dbl]]) { dbls =>
      val materialized = materializeExpr(Expr.UniformChoice(Expr.Lst(dbls)))(emptyCtx).asInstanceOf[Evolution[Double]]
      val materializedDoubles = materialized.run.take(10).toList
      val expectedDoubles = dbls.map(_.d).toSet
      materializedDoubles foreach { d =>
        expectedDoubles should contain(d)
      }
    }

    "should lazily materialize if then else expressions" in {
      val expression = IfThen(Bool(true), Integer(1), Var("IShouldNotBeEvaluated"))
      materializeExpr(expression)(emptyCtx) shouldBe 1
    }

    "should materialize sliding maps" in {
      val mappee = Expr.Cons(Expr.Dbl(1), Expr.Cons(Expr.Dbl(2), Expr.Cons(Expr.Dbl(3), Expr.Empty())))
      val expression = Expr.SlidingMap(
        mappee,
        Expr.Lambda(
          "a",
          Expr.Lambda(
            "b",
            Expr.Add(Expr.Var("a"), Expr.Var("b"), Additive.DoubleDoubleDouble)
          )
        )
      )
      val elems = materializeExpr(expression)(emptyCtx).asInstanceOf[Evolution[Double]].run.take(10).toList

      elems shouldBe List(3, 5)
    }

    "should materialize iterated functions" in {
      val expression = Expr.Iterate(
        Expr.Lambda("x", Expr.Add(Expr.Var("x"), Expr.Dbl(1), Additive.DoubleDoubleDouble)),
        Expr.Dbl(0)
      )
      val elems = materializeExpr(expression)(emptyCtx).asInstanceOf[Evolution[Double]].run.take(5).toList

      elems shouldBe List(0, 1, 2, 3, 4)
    }

    "should materialize trivial fix expressions" in {
      val expression =
        Fix[Int => Int](
          Lambda(
            "self",
            Lambda(
              "n",
              Var("n")
            )
          )
        )

      val factorial = materializeExpr(expression)(emptyCtx).asInstanceOf[Int => Int]
      factorial(3) shouldBe 3
    }

    "should materialize fix expressions" in {
      val expression =
        Fix[Int => Int](
          Lambda(
            "self",
            Lambda(
              "n",
              IfThen(
                LessThanOrEqual(Var("n"), Integer(0), Comparable.Int),
                Integer(1),
                Multiply(
                  Var("n"),
                  App(
                    Var("self"),
                    Minus(Var("n"), Integer(1), Additive.IntIntInt, Invertible.Int)
                  ),
                  Multiplicative.IntIntInt
                )
              )
            )
          )
        )

      val factorial = materializeExpr(expression)(emptyCtx).asInstanceOf[Int => Int]
      factorial(3) shouldBe 6
    }
  }

  val genBooleanLiteral: Gen[Bool] = Gen.oneOf(false, true).map(Bool)
  implicit val arbDouble: Arbitrary[Dbl] = Arbitrary(arbitrary[Double].map(Dbl))

  // TODO are we just replicating the implementation here?
  def genEqualityOperatorExpectations[T](
    eq: Equable[T]
  ): Gen[((Expr[T], Expr[T]) => Expr[Boolean], (T, T) => Boolean)] =
    Gen.oneOf[((Expr[T], Expr[T]) => Expr[Boolean], (T, T) => Boolean)](
      (Equals[T](_, _, eq)) -> MaterializeEquality(eq).eqv _,
      (Neq[T](_, _, eq)) -> MaterializeEquality(eq).neqv _
    )

  def genRelationOperatorExpectations[T](
    cmp: Comparable[T]
  ): Gen[((Expr[T], Expr[T]) => Expr[Boolean], (T, T) => Boolean)] =
    Gen.oneOf[((Expr[T], Expr[T]) => Expr[Boolean], (T, T) => Boolean)](
      (GreaterThan[T](_, _, cmp)) -> MaterializeComparison(cmp).gt _,
      (GreaterThanOrEqual[T](_, _, cmp)) -> MaterializeComparison(cmp).gteqv _,
      (LessThan[T](_, _, cmp)) -> MaterializeComparison(cmp).lt _,
      (LessThanOrEqual[T](_, _, cmp)) -> MaterializeComparison(cmp).lteqv _
    )
}
