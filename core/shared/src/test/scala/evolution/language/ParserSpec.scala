package evolution.language
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{ Gen, Shrink }
import evolution.compiler.ast.AST
import evolution.compiler.phases.parsing.Parser
import evolution.compiler.phases.typing.config.{ Constant0, Constant1, Constant2, Constant3 }

class ParserSpec extends LanguageSpec {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  "The expression parser" - {
    "should parse" - {
      "int literals" in {
        forAll { n: Int =>
          unsafeParse(n.toString) shouldBe AST.IntLiteral(n)
        }
      }

      "doubles literals that are not integers" in {
        forAll { d: Double =>
          whenever(d % 1 != 0) {
            unsafeParse(d.toString) shouldBe AST.DoubleLiteral(d)
          }
        }
      }

      "variables" in {
        forAll(genIdentifier) { varName =>
          unsafeParse(s"$varName") shouldBe AST.Identifier(varName)
        }
      }

      "parse binary operators" in {
        forAll(genLeafExpr, genOperatorWithAST, genLeafExpr) {
          case (a, (op, opAST), b) =>
            unsafeParse(s"$a $op $b") shouldBe AST.AppN(opAST, unsafeParse(a), unsafeParse(b))
        }
      }

      "inverses" in {
        unsafeParse("-point(0, 0)") shouldBe AST.App(
          AST.Const(Constant1.Inverse),
          AST.AppN(AST.Const(Constant2.Point), AST.IntLiteral(0), AST.IntLiteral(0))
        )
      }

      "bindings" - {
        "a = 2 in $a" in {
          forAll(genIdentifier, genLeafExpr) { (id, expr) =>
            unsafeParse(s"$id =$expr in $id") shouldBe AST.Let(id.toLowerCase, unsafeParse(expr), AST.Identifier(id))
          }
        }

        "a = b in\\n 1 + 2" in {
          forAll(genIdentifier, genLeafExpr) { (id, expr) =>
            unsafeParse(s"$id = $expr in 1 + 2") shouldBe AST.Let(
              id.toLowerCase,
              unsafeParse(expr),
              AST.AppN(AST.Const(Constant2.Add), AST.IntLiteral(1), AST.IntLiteral(2))
            )
          }
        }

        "a = aval in b = bval in body" in {
          forAll(genIdentifier, genLeafExpr, genIdentifier, genLeafExpr, genLeafExpr) { (a, aVal, b, bVal, body) =>
            unsafeParse(s"$a = $aVal in $b = $bVal in $body") shouldBe
              AST.Let(a.toLowerCase, unsafeParse(aVal), AST.Let(b.toLowerCase, unsafeParse(bVal), unsafeParse(body)))
          }
        }
      }

      "sampling" - {
        "a <- b in a" in {
          unsafeParse("a <- b in c") shouldBe
            AST.AppN(AST.Const(Constant2.WithFirst), AST.Identifier("b"), AST.Lambda("a", AST.Identifier("c")))
        }
      }

      "a * b + c = (a * b) + c" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a * $b + $c") shouldBe AST.AppN(
            AST.Const(Constant2.Add),
            AST.AppN(AST.Const(Constant2.Multiply), unsafeParse(a), unsafeParse(b)),
            unsafeParse(c)
          )
        }
      }

      "a + b * c = a + (b * c)" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a + $b * $c") shouldBe AST.AppN(
            AST.Const(Constant2.Add),
            unsafeParse(a),
            AST.AppN(AST.Const(Constant2.Multiply), unsafeParse(b), unsafeParse(c))
          )
        }
      }

      "(a + b) * c" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"($a + $b) * $c") shouldBe AST.AppN(
            AST.Const(Constant2.Multiply),
            AST.AppN(AST.Const(Constant2.Add), unsafeParse(a), unsafeParse(b)),
            unsafeParse(c)
          )
        }
      }

      "lambdas" in {
        forAll(genIdentifier, genLeafExpr) { (identifier, expr) =>
          unsafeParse(s"$identifier -> $expr") shouldBe AST.Lambda(identifier.toLowerCase, unsafeParse(expr))
        }
      }

      "HO lambdas" in {
        forAll(genIdentifier, genIdentifier, genLeafExpr) { (identifier1, identifier2, expr) =>
          unsafeParse(s"$identifier1 -> $identifier2 ->$expr") shouldBe AST.Lambda(
            identifier1.toLowerCase,
            AST.Lambda(identifier2.toLowerCase, unsafeParse(expr))
          )
        }
      }

      "Let bindings" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (id, value, in) =>
          unsafeParse(s"$id = $value in $in") shouldBe AST.Let(id.toLowerCase, unsafeParse(value), unsafeParse(in))
        }
      }

      "a -> b + c = a -> (b + c)" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (identifier1, expr1, expr2) =>
          unsafeParse(s"$identifier1 -> $expr1 + $expr2") shouldBe AST.Lambda(
            identifier1.toLowerCase,
            AST.AppN(AST.Const(Constant2.Add), unsafeParse(expr1), unsafeParse(expr2))
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
          unsafeParse(s"$identifier1($expr1, $expr2)") shouldBe AST.AppN(
            AST.Identifier(identifier1),
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
        "2^3 + 1" in {
          unsafeParse("2^3 + 1") shouldBe AST.AppN(
            AST.Const(Constant2.Add),
            AST.AppN(AST.Const(Constant2.Exp), AST.IntLiteral(2), AST.IntLiteral(3)),
            AST.IntLiteral(1)
          )
        }

        "2^3 * 2" in {
          unsafeParse("2^3 * 2") shouldBe AST.AppN(
            AST.Const(Constant2.Multiply),
            AST.AppN(AST.Const(Constant2.Exp), AST.IntLiteral(2), AST.IntLiteral(3)),
            AST.IntLiteral(2)
          )
        }

        "2 * 2^3" in {
          unsafeParse("2 * 2^3") shouldBe AST.AppN(
            AST.Const(Constant2.Multiply),
            AST.IntLiteral(2),
            AST.AppN(AST.Const(Constant2.Exp), AST.IntLiteral(2), AST.IntLiteral(3))
          )
        }
      }

      "divisions" - {
        "a / b + c = (a / b) + c" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"$a / $b + $c") shouldBe AST.AppN(
              AST.Const(Constant2.Add),
              AST.AppN(AST.Const(Constant2.Div), unsafeParse(a), unsafeParse(b)),
              unsafeParse(c)
            )
          }
        }

        "a + b / c = a + (b / c)" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"$a + $b / $c") shouldBe AST.AppN(
              AST.Const(Constant2.Add),
              unsafeParse(a),
              AST.AppN(AST.Const(Constant2.Div), unsafeParse(b), unsafeParse(c))
            )
          }
        }

        "(a + b) / c" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"($a + $b) / $c") shouldBe AST.AppN(
              AST.Const(Constant2.Div),
              AST.AppN(AST.Const(Constant2.Add), unsafeParse(a), unsafeParse(b)),
              unsafeParse(c)
            )
          }
        }

        "@point(a, b)" in {
          forAll(genLeafExpr, genLeafExpr) { (a, b) =>
            unsafeParse(s"@point($a, $b)") shouldBe AST.AppN(
              AST.Const(Constant2.LiftedPoint),
              unsafeParse(a),
              unsafeParse(b)
            )
          }
        }

        "const(expr(a, b))" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (expr, a, b) =>
            unsafeParse(s"const($expr($a, $b))") shouldBe AST.App(
              AST.Const(Constant1.Constant),
              AST.AppN(unsafeParse(expr), unsafeParse(a), unsafeParse(b))
            )
          }
        }

        "const(n)" in {
          forAll(arbitrary[Double]) { d =>
            unsafeParse(s"const($d)") shouldBe
              AST.App(AST.Const(Constant1.Constant), unsafeParse(d.toString))
          }
        }

        "[a, b, c]" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"[$a, $b, $c]") shouldBe AST.AppN(
              AST.Const(Constant2.Cons),
              unsafeParse(a),
              AST.AppN(
                AST.Const(Constant2.Cons),
                unsafeParse(b),
                AST.AppN(AST.Const(Constant2.Cons), unsafeParse(c), AST.Const(Constant0.Empty))
              )
            )
          }
        }
      }

      "variadic zipWith(a, b, c, f)" in {
        pending
        forAll(genLeafExpr, genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c, f) =>
          val parsed = unsafeParse(s"zipWith($a, $b, $c, $f)")
          val expected = AST.AppN(
            AST.Const(Constant3.ZipWith),
            AST.AppN(
              AST.Const(Constant3.ZipWith),
              unsafeParse(a),
              unsafeParse(b),
              unsafeParse(f)
            ),
            unsafeParse(c),
            AST.Lambda(
              "f",
              AST.Lambda(
                "x",
                AST.App(AST.Identifier("f"), AST.Identifier("x"))
              )
            )
          )

          parsed shouldBe expected
        }
      }

      "zip(a <- as, b <- bs, c <- cs) in d" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c, d) =>
          val parsed = unsafeParse(s"zip(a <- $a, b <- $b, c <- $c) in $d")
          val lambda = AST.Lambda("a", AST.Lambda("b", AST.Lambda("c", unsafeParse(d))))
          val expected = AST.AppN(
            AST.Const(Constant3.ZipWith),
            AST.AppN(
              AST.Const(Constant3.ZipWith),
              unsafeParse(a),
              unsafeParse(b),
              lambda
            ),
            unsafeParse(c),
            AST.Lambda(
              "f",
              AST.Lambda(
                "x",
                AST.App(AST.Identifier("f"), AST.Identifier("x"))
              )
            )
          )

          parsed shouldBe expected
        }
      }

      "product(a <- as, b <- bs) in d" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          val parsed = unsafeParse(s"product(a <- $a, b <- $b) in $c")
          val expected =
            AST.AppN(
              AST.Const(Constant2.FlatMap),
              unsafeParse(a),
              AST.Lambda(
                "a",
                AST.AppN(
                  AST.Const(Constant2.Map),
                  unsafeParse(b),
                  AST.Lambda(
                    "b",
                    unsafeParse(c)
                  )
                )
              )
            )

          parsed shouldBe expected
        }
      }

      "boolean literals" in {
        unsafeParse("true") shouldBe AST.Bool(true)
        unsafeParse("false") shouldBe AST.Bool(false)
      }

      "not" in {
        forAll(genLeafExpr) { a =>
          unsafeParse(s"!$a") shouldBe
            AST.App(AST.Const(Constant1.Not), unsafeParse(a))
        }
      }

      "logical operators with the right precedence" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a || $b && $c") shouldBe
            AST.AppN(
              AST.Const(Constant2.Or),
              unsafeParse(a),
              AST.AppN(AST.Const(Constant2.And), unsafeParse(b), unsafeParse(c))
            )
        }
      }
    }

    "when it fails" - {
      "it should report the line number" in {
        val expr = """|a    = 10 in
                      |g(a a)
                   """.stripMargin

        val failure = Parser.parse(expr)
        // The first line is 0
        val lineNumber = failure.left.map(_.lineNumber)
        lineNumber shouldBe Left(1)
      }
    }
  }

  def unsafeParse(string: String): AST = Parser.parse(string).toTry.get
}
