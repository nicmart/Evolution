package evolution.language
import cats.Id
import evolution.language.InterpreterModule.Constant
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{ Gen, Shrink }

class ParserModuleSpec extends LanguageSpec[Id] {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  "The expression parser" - {
    "should parse" - {
      "doubles literals" in {
        forAll { d: Double =>
          unsafeParse(d.toString) shouldBe AST.Number(d.toString)
        }
      }

      "variables" in {
        forAll(genIdentifier) { varName =>
          unsafeParse(s"$$$varName") shouldBe AST.Var(varName)
        }
      }

      "additions" in {
        forAll(genLeafExpr, genLeafExpr) { (a, b) =>
          unsafeParse(s"$a + $b") shouldBe AST.App2(AST.Const(Constant.Add), unsafeParse(a), unsafeParse(b))
        }
      }

      "inverses" in {
        unsafeParse("-point(0, 0)") shouldBe AST.App(
          AST.Const(Constant.Inverse),
          AST.App2(AST.Const(Constant.Point), AST.Number("0"), AST.Number("0")))
      }

      "bindings" - {
        "a = 2 in $a" in {
          forAll(genIdentifier, genLeafExpr) { (id, expr) =>
            unsafeParse(s"$id =$expr in $$$id") shouldBe AST.Let(AST.Var(id), unsafeParse(expr), AST.Var(id))
          }
        }

        "a = b in\\n 1 + 2" in {
          forAll(genIdentifier, genLeafExpr) { (id, expr) =>
            unsafeParse(s"$id = $expr in 1 + 2") shouldBe AST.Let(
              AST.Var(id),
              unsafeParse(expr),
              AST.App2(AST.Const(Constant.Add), AST.Number("1"), AST.Number("2")))
          }
        }

        "a = aval in b = bval in body" in {
          forAll(genIdentifier, genLeafExpr, genIdentifier, genLeafExpr, genLeafExpr) { (a, aVal, b, bVal, body) =>
            unsafeParse(s"$a = $aVal in $b = $bVal in $body") shouldBe
              AST.Let(AST.Var(a), unsafeParse(aVal), AST.Let(AST.Var(b), unsafeParse(bVal), unsafeParse(body)))
          }
        }
      }

      "multiplications" in {
        forAll(genLeafExpr, genLeafExpr) { (a, b) =>
          unsafeParse(s"$a * $b") shouldBe AST.App2(AST.Const(Constant.Multiply), unsafeParse(a), unsafeParse(b))
        }
      }

      "mods" in {
        forAll(genLeafExpr, genLeafExpr) { (a, b) =>
          unsafeParse(s"$a % $b") shouldBe AST.App2(AST.Const(Constant.Mod), unsafeParse(a), unsafeParse(b))
        }
      }

      "a * b + c = (a * b) + c" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a * $b + $c") shouldBe AST.App2(
            AST.Const(Constant.Add),
            AST.App2(AST.Const(Constant.Multiply), unsafeParse(a), unsafeParse(b)),
            unsafeParse(c)
          )
        }
      }

      "a + b * c = a + (b * c)" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a + $b * $c") shouldBe AST.App2(
            AST.Const(Constant.Add),
            unsafeParse(a),
            AST.App2(AST.Const(Constant.Multiply), unsafeParse(b), unsafeParse(c))
          )
        }
      }

      "(a + b) * c" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"($a + $b) * $c") shouldBe AST.App2(
            AST.Const(Constant.Multiply),
            AST.App2(AST.Const(Constant.Add), unsafeParse(a), unsafeParse(b)),
            unsafeParse(c)
          )
        }
      }

      "lambdas" in {
        forAll(genIdentifier, genLeafExpr) { (identifier, expr) =>
          unsafeParse(s"$identifier -> $expr") shouldBe AST.Lambda(AST.Var(identifier), unsafeParse(expr))
        }
      }

      "HO lambdas" in {
        forAll(genIdentifier, genIdentifier, genLeafExpr) { (identifier1, identifier2, expr) =>
          unsafeParse(s"$identifier1 -> $identifier2 ->$expr") shouldBe AST.Lambda(
            AST.Var(identifier1),
            AST.Lambda(AST.Var(identifier2), unsafeParse(expr))
          )
        }
      }

      "Let bindings" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (id, value, in) =>
          unsafeParse(s"let($id, $value, $in)") shouldBe AST.Let(AST.Var(id), unsafeParse(value), unsafeParse(in))
        }
      }

      "a -> b + c = a -> (b + c)" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (identifier1, expr1, expr2) =>
          unsafeParse(s"$identifier1 -> $expr1 + $expr2") shouldBe AST.Lambda(
            AST.Var(identifier1),
            AST.App2(AST.Const(Constant.Add), unsafeParse(expr1), unsafeParse(expr2))
          )
        }
      }

      "expressions with whitespaces at the beginning and at the end" in {
        forAll(genWhitespace, genLeafExpr, genWhitespace) { (wsStart, expr, wsEnd) =>
          unsafeParse(s"$wsStart$expr$wsEnd") shouldBe unsafeParse(expr)
        }
      }

      "ignore comments" in {
        forAll(genLeafExpr, Gen.alphaNumStr, Gen.alphaNumStr) { (expr, comment1, comment2) =>
          unsafeParse(s"//$comment1\n$expr\n//$comment2") shouldBe unsafeParse(expr)
        }

      }

      "parse applications of vars" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (identifier1, expr1, expr2) =>
          unsafeParse(s"$$$identifier1($expr1, $expr2)") shouldBe AST.App2(
            AST.Var(identifier1),
            unsafeParse(expr1),
            unsafeParse(expr2)
          )
        }
      }

      "parse applications of lambdas" in {
        forAll(genLambda, genLeafExpr) { (lambda, expr) =>
          unsafeParse(s"($lambda)($expr)") shouldBe AST.App(
            unsafeParse(lambda),
            unsafeParse(expr)
          )
        }
      }

      "parse exponentials" - {
        "2^3" in {
          unsafeParse("2^3") shouldBe AST.App2(AST.Const(Constant.Exp), AST.Number("2"), AST.Number("3"))
        }

        "2^3 + 1" in {
          unsafeParse("2^3 + 1") shouldBe AST.App2(
            AST.Const(Constant.Add),
            AST.App2(AST.Const(Constant.Exp), AST.Number("2"), AST.Number("3")),
            AST.Number("1")
          )
        }

        "2^3 * 2" in {
          unsafeParse("2^3 * 2") shouldBe AST.App2(
            AST.Const(Constant.Multiply),
            AST.App2(AST.Const(Constant.Exp), AST.Number("2"), AST.Number("3")),
            AST.Number("2")
          )
        }

        "2 * 2^3" in {
          unsafeParse("2 * 2^3") shouldBe AST.App2(
            AST.Const(Constant.Multiply),
            AST.Number("2"),
            AST.App2(AST.Const(Constant.Exp), AST.Number("2"), AST.Number("3"))
          )
        }
      }

      "parse divisions" - {
        "a / b + c = (a / b) + c" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"$a / $b + $c") shouldBe AST.App2(
              AST.Const(Constant.Add),
              AST.App2(AST.Const(Constant.Div), unsafeParse(a), unsafeParse(b)),
              unsafeParse(c)
            )
          }
        }

        "a + b / c = a + (b / c)" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"$a + $b / $c") shouldBe AST.App2(
              AST.Const(Constant.Add),
              unsafeParse(a),
              AST.App2(AST.Const(Constant.Div), unsafeParse(b), unsafeParse(c))
            )
          }
        }

        "(a + b) / c" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"($a + $b) / $c") shouldBe AST.App2(
              AST.Const(Constant.Div),
              AST.App2(AST.Const(Constant.Add), unsafeParse(a), unsafeParse(b)),
              unsafeParse(c)
            )
          }
        }

        "<expr>(a, b)" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (expr, a, b) =>
            unsafeParse(s"<$expr>($a, $b)") shouldBe AST.App2(
              AST.App(AST.Const(Constant.Lift), unsafeParse(expr)),
              unsafeParse(a),
              unsafeParse(b))
          }
        }

        "<n>" in {
          forAll(arbitrary[Double]) { d =>
            unsafeParse(s"<$d>") shouldBe
              AST.App(AST.Const(Constant.Lift), unsafeParse(d.toString))
          }
        }

        "1 <+> 2" in {
          unsafeParse("1 <+> 2") shouldBe AST.App2(
            AST.App(AST.Const(Constant.Lift), AST.Const(Constant.Add)),
            AST.Number("1"),
            AST.Number("2")
          )
        }

        "[a, b, c]" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"[$a, $b, $c]") shouldBe AST.App2(
              AST.Const(Constant.Cons),
              unsafeParse(a),
              AST.App2(
                AST.Const(Constant.Cons),
                unsafeParse(b),
                AST.App2(AST.Const(Constant.Cons), unsafeParse(c), AST.Const(Constant.Empty)))
            )
          }
        }
      }

      "parse ands" in {
        forAll(genLeafExpr, genLeafExpr) { (a, b) =>
          unsafeParse(s"$a && $b") shouldBe
            AST.App2(AST.Const(Constant.And), unsafeParse(a), unsafeParse(b))
        }
      }

      "parse ors" in {
        forAll(genLeafExpr, genLeafExpr) { (a, b) =>
          unsafeParse(s"$a || $b") shouldBe
            AST.App2(AST.Const(Constant.Or), unsafeParse(a), unsafeParse(b))
        }
      }

      "parse not" in {
        forAll(genLeafExpr) { a =>
          unsafeParse(s"!$a") shouldBe
            AST.App(AST.Const(Constant.Not), unsafeParse(a))
        }
      }

      "parse logical operators with the right precedence" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a || $b && $c") shouldBe
            AST.App2(
              AST.Const(Constant.Or),
              unsafeParse(a),
              AST.App2(AST.Const(Constant.And), unsafeParse(b), unsafeParse(c)))
        }
      }
    }
  }

  def unsafeParse(string: String): AST = Parsers.parser.parse(string).get.value
}
