package evolution.primitive
import evolution.primitive.ast.Expr
import org.scalacheck.{ Gen, Shrink }
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ASTParserSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  "The expression parser" - {
    "should parse" - {
      "doubles literals" in {
        forAll { d: Double =>
          unsafeParse(d.toString) shouldBe Expr.Number(d.toString)
        }
      }

      "variables" in {
        forAll(genIdentifier) { varName =>
          unsafeParse(s"$$$varName") shouldBe Expr.Var(varName)
        }
      }

      "additions" in {
        forAll(genLeafExpr, genLeafExpr) { (a, b) =>
          unsafeParse(s"$a + $b") shouldBe Expr.BinaryOp("+", unsafeParse(a), unsafeParse(b))
        }
      }

      "multiplications" in {
        forAll(genLeafExpr, genLeafExpr) { (a, b) =>
          unsafeParse(s"$a * $b") shouldBe Expr.BinaryOp("*", unsafeParse(a), unsafeParse(b))
        }
      }

      "a * b + c = (a * b) + c" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a * $b + $c") shouldBe Expr.BinaryOp(
            "+",
            Expr.BinaryOp("*", unsafeParse(a), unsafeParse(b)),
            unsafeParse(c))
        }
      }

      "a + b * c = a + (b * c)" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a + $b * $c") shouldBe Expr.BinaryOp(
            "+",
            unsafeParse(a),
            Expr.BinaryOp("*", unsafeParse(b), unsafeParse(c))
          )
        }
      }

      "function calls" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (f, a, b) =>
          unsafeParse(s"$f($a, $b)") shouldBe Expr.FuncCall(
            f,
            List(unsafeParse(a), unsafeParse(b))
          )
        }
      }
    }
  }

  def genLeafExpr: Gen[String] =
    Gen.oneOf(genVarUsage, arbitrary[Double].map(_.toString))

  def genVarUsage: Gen[String] = genIdentifier.map(v => s"$$$v")

  def genIdentifier: Gen[String] = for {
    head <- Gen.alphaChar
    tail <- Gen.alphaNumStr
  } yield head + tail

  def unsafeParse(string: String): Expr = parser.parser.parse(string).get.value
}
