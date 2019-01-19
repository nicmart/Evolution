package evolution.primitive
import evolution.primitive.ast.Expr
import org.scalatest.{ FreeSpec, Matchers }
import parser.expr
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ParserSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  "The expression parser" - {
    "should parse doubles literals" in {
      forAll { d: Double =>
        unsafeParse(d.toString) shouldBe Expr.Dbl(d)
      }
    }
  }

  def unsafeParse(string: String): Expr = expr.parse(string).get.value
}
