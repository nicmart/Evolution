package evolution.primitive
import cats.Id
import org.scalacheck.{ Gen, Shrink }
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ASTParserSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
  val parser = new Parsers[Id](new Ast[Id])
  import parser.ast._
  import PredefinedFunction._
  import Expr._

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
          unsafeParse(s"$a + $b") shouldBe Expr.FuncCall(Add, List(unsafeParse(a), unsafeParse(b)))
        }
      }

      "multiplications" in {
        forAll(genLeafExpr, genLeafExpr) { (a, b) =>
          unsafeParse(s"$a * $b") shouldBe Expr.FuncCall(Multiply, List(unsafeParse(a), unsafeParse(b)))
        }
      }

      "a * b + c = (a * b) + c" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a * $b + $c") shouldBe Expr.FuncCall(
            Add,
            List(Expr.FuncCall(Multiply, List(unsafeParse(a), unsafeParse(b))), unsafeParse(c)))
        }
      }

      "a + b * c = a + (b * c)" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a + $b * $c") shouldBe Expr.FuncCall(
            Add,
            List(unsafeParse(a), Expr.FuncCall(Multiply, List(unsafeParse(b), unsafeParse(c))))
          )
        }
      }

      "(a + b) * c" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"($a + $b) * $c") shouldBe Expr.FuncCall(
            Multiply,
            List(Expr.FuncCall(Add, List(unsafeParse(a), unsafeParse(b))), unsafeParse(c))
          )
        }
      }

      "function calls" in {
        forAll(genPredefinedFunc, genFunctionArgs) { (f, args) =>
          val expr = s"${f.entryName}(${args.mkString(", ")})"
          val expected = Expr.FuncCall(f, args.map(unsafeParse))
          unsafeParse(expr) shouldBe expected
        }
      }

      "lambdas" in {
        forAll(genIdentifier, genLeafExpr) { (identifier, expr) =>
          unsafeParse(s"$identifier -> $expr") shouldBe Expr.Lambda(Expr.Var(identifier), unsafeParse(expr))
        }
      }

      "HO lambdas" in {
        forAll(genIdentifier, genIdentifier, genLeafExpr) { (identifier1, identifier2, expr) =>
          unsafeParse(s"$identifier1 -> $identifier2 ->$expr") shouldBe Expr.Lambda(
            Expr.Var(identifier1),
            Expr.Lambda(Expr.Var(identifier2), unsafeParse(expr)))
        }
      }

      "Let bindings" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (id, value, in) =>
          unsafeParse(s"let($id, $value, $in)") shouldBe Let(Var(id), unsafeParse(value), unsafeParse(in))
        }
      }

      "a -> b + c = a -> (b + c)" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (identifier1, expr1, expr2) =>
          unsafeParse(s"$identifier1 -> $expr1 + $expr2") shouldBe Expr.Lambda(
            Expr.Var(identifier1),
            Expr.FuncCall(Add, List(unsafeParse(expr1), unsafeParse(expr2))))
        }
      }

      "Parse a complex expression: a -> b -> ($c + 2) * app($d, inverse(-1))" in {
        val parsed = unsafeParse("a -> b -> ($c + 2) * app($d, inverse(-1))")
        val expected =
          Expr.Lambda(
            Expr.Var("a"),
            Expr.Lambda(
              Expr.Var("b"),
              Expr.FuncCall(
                Multiply,
                List(
                  Expr.FuncCall(Add, List(Expr.Var("c"), Expr.Number("2"))),
                  Expr.FuncCall(App, List(Expr.Var("d"), Expr.FuncCall(Inverse, List(Expr.Number("-1"))))))
              )
            )
          )
        parsed shouldBe expected
      }
    }
  }

  def genFunctionArgs: Gen[List[String]] =
    for {
      n <- Gen.choose(1, 6)
      args <- Gen.listOfN(n, genLeafExpr)
    } yield args

  def genLeafExpr: Gen[String] =
    Gen.oneOf(genVarUsage, arbitrary[Double].map(_.toString))

  def genVarUsage: Gen[String] = genIdentifier.map(v => s"$$$v")

  def genIdentifier: Gen[String] = for {
    head <- Gen.alphaChar
    tail <- Gen.alphaNumStr
  } yield head + tail

  def genPredefinedFunc: Gen[PredefinedFunction] =
    Gen.oneOf(PredefinedFunction.values)

  def unsafeParse(string: String): Expr = parser.parser.parse(string).get.value
}
