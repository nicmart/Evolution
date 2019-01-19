package evolution.primitive
import evolution.primitive.ast.Expr
import org.scalacheck.{ Gen, Shrink }
import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ASTParserSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  "The expression parser" - {
    "should parse doubles literals" in {
      forAll { d: Double =>
        unsafeParse(d.toString) shouldBe Expr.Number(d.toString)
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

  def unsafeParse(string: String): Expr = parser.parser.parse(string).get.value
}
