package evolution.compiler
import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalactic.Prettifier
import pprint.PPrinter
import org.scalactic.TypeCheckedTripleEquals
import org.scalactic.Equality

trait LanguageSpec
    extends FreeSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with TreeArbitraries
    with TypeCheckedTripleEquals {
  implicit class EitherOps[T](t: Either[String, T]) {
    def unsafeRight: T =
      t.fold(
        s => throw new Exception(s),
        identity
      )
  }

  implicit class ShouldEqOps[A: Equality](left: A) {
    def shouldEq(right: A) = left shouldEqual right
  }

  implicit val pprinterPrettifier = new Prettifier {
    def apply(o: Any): String = PPrinter.BlackWhite.apply(o, height = Int.MaxValue).toString()
  }
}
