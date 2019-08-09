package evolution.language
import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait LanguageSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks with ASTArbitraries {
  implicit class EitherOps[T](t: Either[String, T]) {
    def unsafeEvaluate: T =
      t.fold(
        s => throw new Exception(s),
        identity
      )
  }

}
