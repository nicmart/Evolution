package evolution.primitive
import evolution.primitive.ast.Expr
import org.scalacheck.{ Gen, Shrink }
import org.scalatest.{ FreeSpec, Matchers }
import parser.expr
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ParserSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit val disableShrink: Shrink[String] = Shrink(s => Stream.empty)
  "The expression parser" - {
    "should parse doubles literals" in {
      forAll { d: Double =>
        unsafeParse(d.toString) shouldBe Expr.Dbl(d)
      }
    }

    "should parse variables" in {
      forAll(genVarName) { varName =>
        unsafeParse(s"$$$varName") shouldBe Expr.Var(varName)
      }
    }
  }

  val genVarName: Gen[String] = for {
    head <- Gen.alphaChar
    tail <- Gen.alphaNumStr
  } yield head + tail

  def unsafeParse(string: String): Expr = expr.parse(string).get.value
}
