package evolution.language
import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalactic.Prettifier
import pprint.PPrinter

trait LanguageSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks with ASTArbitraries {
  implicit class EitherOps[T](t: Either[String, T]) {
    def unsafeEvaluate: T =
      t.fold(
        s => throw new Exception(s),
        identity
      )
  }

  implicit val pprinterPrettifier = new Prettifier {
    def apply(o: Any): String = PPrinter.BlackWhite.apply(o, height = Int.MaxValue).toString()
  }
}
