package evolution.language
import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait LanguageSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks with ASTArbitraries
