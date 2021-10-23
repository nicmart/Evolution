package evolution.compiler
import evolution.compiler.term.Term
import evolution.compiler.term.Term.Let
import org.scalactic.{Equality, Prettifier, TypeCheckedTripleEquals}
import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import pprint.PPrinter

trait LanguageSpec
    extends AnyFreeSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with TreeArbitraries
    with TypeCheckedTripleEquals {
  implicit class EitherOps[T](t: Either[String, T]) {
    def unsafeRight: T =
      t.fold(
        s => throw new Exception(s),
        identity
      )

    def unsafeLeft: String =
      t.fold(
        identity,
        s => throw new Exception(s"Getting a Left on a Right($s)")
      )
  }

  implicit class ShouldEqOps[A: Equality](left: A) {
    def shouldEq(right: A): Assertion = left shouldEqual right
  }

  implicit val pprinterPrettifier: Prettifier = new Prettifier {
    def apply(o: Any): String = PPrinter.BlackWhite.apply(o, height = Int.MaxValue).toString()
  }

  def lets(terms: (String, Term)*)(in: Term): Term =
    terms.foldRight(in) { case ((name, definition), term) => Let(name, definition, term) }
}
