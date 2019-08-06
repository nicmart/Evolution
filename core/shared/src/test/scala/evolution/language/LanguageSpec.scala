package evolution.language
import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait LanguageSpec[F[_]] extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks with ASTArbitraries[F]
