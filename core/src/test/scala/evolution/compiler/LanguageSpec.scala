package evolution.compiler
import evolution.compiler.term.Term
import evolution.compiler.term.Term.Let
import org.scalactic.{Equality, Prettifier, TypeCheckedTripleEquals}
import org.scalatest.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import pprint.PPrinter

trait LanguageSpec
    extends AnyFreeSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with TreeArbitraries
    with TypeCheckedTripleEquals:
  extension [T](t: Either[String, T])
    def unsafeRight: T =
      t.fold(
        s => throw Exception(s),
        identity
      )
    def unsafeLeft: String =
      t.fold(
        identity,
        s => throw Exception(s"Getting a Left on a Right($s)")
      )

  extension [A: Equality](left: A) def shouldEq(right: A): Assertion = left `shouldEqual` right

  given Prettifier with
    def apply(o: Any): String = PPrinter.BlackWhite.apply(o, height = Int.MaxValue).toString()

  def lets(terms: (String, Term)*)(in: Term): Term =
    terms.foldRight(in) { case ((name, definition), term) => Let(name, definition, term) }
