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

class InterpreterModuleSpec extends FreeSpec with GeneratorDrivenPropertyChecks with Matchers {
  // TODO Rubbish, rubbish, rubbish!!!
  val interpreter = new InterpreterModule {}
  import interpreter.Interpreter._

  "The interpreter" - {
    "should interpret Pnt" in {
      interpret(Pnt(Dbl(0), Dbl(0)))(emptyCtx) shouldBe Point(0, 0)
    }

    "should interpret booleans literals" in forAll(genBooleanLiteral) { literal =>
      interpret(literal)(emptyCtx) shouldBe literal.b
    }

    "should interpret ands" in forAll(genBooleanLiteral, genBooleanLiteral) { (a, b) =>
      interpret(And(a, b))(emptyCtx) shouldBe a.b && b.b
    }

    "should interpret ors" in forAll(genBooleanLiteral, genBooleanLiteral) { (a, b) =>
      interpret(Or(a, b))(emptyCtx) shouldBe a.b || b.b
    }

    "should interpret nots" in forAll(genBooleanLiteral) { a =>
      interpret(Not(a))(emptyCtx) shouldBe !a.b
    }

    "should interpret inRect statements" in {
      interpret(InRect(Pnt(Dbl(0), Dbl(0)), Pnt(Dbl(10), Dbl(10)), Pnt(Dbl(5), Dbl(5))))(emptyCtx) shouldBe true
      interpret(InRect(Pnt(Dbl(0), Dbl(0)), Pnt(Dbl(10), Dbl(10)), Pnt(Dbl(20), Dbl(5))))(emptyCtx) shouldBe false
    }

    "should interpret relational operators" in forAll(
      genRelationOperatorExpectations[Double],
      arbitrary[Dbl],
      arbitrary[Dbl]
    ) {
      case ((op, expected), a, b) =>
        interpret(op(a, b))(emptyCtx) shouldBe expected(interpret(a)(emptyCtx), interpret(b)(emptyCtx))
    }

    "should interpret equality operators" in forAll(
      genEqualityOperatorExpectations[Double],
      arbitrary[Dbl],
      arbitrary[Dbl]
    ) {
      case ((op, expected), a, b) =>
        interpret(op(a, b))(emptyCtx) shouldBe expected(interpret(a)(emptyCtx), interpret(b)(emptyCtx))
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
