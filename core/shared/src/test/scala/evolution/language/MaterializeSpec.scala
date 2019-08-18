package evolution.language
import cats.Eq
import cats.implicits._
import cats.kernel.Order
import evolution.data.EvaluationContext._
import evolution.geometry.Point
import evolution.data.Expr
import evolution.data.Expr._
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ FreeSpec, Matchers }
import evolution.compiler.phases.materializing.Materialize.materialize
import evolution.typeclass.Semigroupoid
import evolution.typeclass.Invertible

class MaterializeSpec extends FreeSpec with GeneratorDrivenPropertyChecks with Matchers {

  "The materializer" - {
    "should materialize Pnt" in {
      materialize(Pnt(Dbl(0), Dbl(0)))(emptyCtx) shouldBe Point(0, 0)
    }

    "should materialize booleans literals" in forAll(genBooleanLiteral) { literal =>
      materialize(literal)(emptyCtx) shouldBe literal.b
    }

    "should materialize ands" in forAll(genBooleanLiteral, genBooleanLiteral) { (a, b) =>
      materialize(And(a, b))(emptyCtx) shouldBe a.b && b.b
    }

    "should materialize ors" in forAll(genBooleanLiteral, genBooleanLiteral) { (a, b) =>
      materialize(Or(a, b))(emptyCtx) shouldBe a.b || b.b
    }

    "should materialize nots" in forAll(genBooleanLiteral) { a =>
      materialize(Not(a))(emptyCtx) shouldBe !a.b
    }

    "should materialize inRect statements" in {
      materialize(InRect(Pnt(Dbl(0), Dbl(0)), Pnt(Dbl(10), Dbl(10)), Pnt(Dbl(5), Dbl(5))))(emptyCtx) shouldBe true
      materialize(InRect(Pnt(Dbl(0), Dbl(0)), Pnt(Dbl(10), Dbl(10)), Pnt(Dbl(20), Dbl(5))))(emptyCtx) shouldBe false
    }

    "should materialize relational operators" in forAll(
      genRelationOperatorExpectations[Double],
      arbitrary[Dbl],
      arbitrary[Dbl]
    ) {
      case ((op, expected), a, b) =>
        materialize(op(a, b))(emptyCtx) shouldBe expected(materialize(a)(emptyCtx), materialize(b)(emptyCtx))
    }

    "should materialize equality operators" in forAll(
      genEqualityOperatorExpectations[Double],
      arbitrary[Dbl],
      arbitrary[Dbl]
    ) {
      case ((op, expected), a, b) =>
        materialize(op(a, b))(emptyCtx) shouldBe expected(materialize(a)(emptyCtx), materialize(b)(emptyCtx))
    }

    "should lazily materialize if then else expressions" in {
      val expression = IfThen(Bool(true), Integer(1), Var("IShouldNotBeEvaluated"))
      materialize(expression)(emptyCtx) shouldBe 1
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

      val factorial = materialize(expression)(emptyCtx).asInstanceOf[Int => Int]
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
                LessThanOrEqual(Var("n"), Integer(0), Order[Int]),
                Integer(1),
                Multiply(
                  Var("n"),
                  App(
                    Var("self"),
                    Minus(Var("n"), Integer(1), Semigroupoid.Additive.intIntInt, Invertible.Additive.intInvertible)
                  ),
                  Semigroupoid.Multiplicative.intIntInt
                )
              )
            )
          )
        )

      val factorial = materialize(expression)(emptyCtx).asInstanceOf[Int => Int]
      factorial(3) shouldBe 6
    }
  }

  val genBooleanLiteral: Gen[Bool] = Gen.oneOf(false, true).map(Bool)
  implicit val arbDouble: Arbitrary[Dbl] = Arbitrary(arbitrary[Double].map(Dbl))

  // TODO are we just replicating the implementation here?
  def genEqualityOperatorExpectations[T](
    implicit eq: Eq[T]
  ): Gen[((Expr[T], Expr[T]) => Expr[Boolean], (T, T) => Boolean)] =
    Gen.oneOf[((Expr[T], Expr[T]) => Expr[Boolean], (T, T) => Boolean)](
      (Equals[T](_, _, Eq[T])) -> eq.eqv _,
      (Neq[T](_, _, Eq[T])) -> eq.neqv _
    )

  def genRelationOperatorExpectations[T](
    implicit ord: Order[T]
  ): Gen[((Expr[T], Expr[T]) => Expr[Boolean], (T, T) => Boolean)] =
    Gen.oneOf[((Expr[T], Expr[T]) => Expr[Boolean], (T, T) => Boolean)](
      (GreaterThan[T](_, _, Order[T])) -> ord.gt _,
      (GreaterThanOrEqual[T](_, _, Order[T])) -> ord.gteqv _,
      (LessThan[T](_, _, Order[T])) -> ord.lt _,
      (LessThanOrEqual[T](_, _, Order[T])) -> ord.lteqv _
    )
}
