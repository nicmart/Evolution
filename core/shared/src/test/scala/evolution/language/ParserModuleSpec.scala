package evolution.language
import cats.Id
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
          unsafeParse(s"$varName") shouldBe AST.Identifier(varName)
        }
      }

      "parse binary operators" in {
        forAll(genLeafExpr, genOperatorWithAST, genLeafExpr) {
          case (a, (op, opAST), b) =>
            unsafeParse(s"$a $op $b") shouldBe AST.App2(opAST, unsafeParse(a), unsafeParse(b))
        }
      }

      "inverses" in {
        unsafeParse("-point(0, 0)") shouldBe AST.App(
          AST.Const(Constant1.Inverse),
          AST.App2(AST.Const(Constant2.Point), AST.Number("0"), AST.Number("0")))
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
              AST.App2(AST.Const(Constant2.Add), AST.Number("1"), AST.Number("2")))
          }
        }

        "a = aval in b = bval in body" in {
          forAll(genIdentifier, genLeafExpr, genIdentifier, genLeafExpr, genLeafExpr) { (a, aVal, b, bVal, body) =>
            unsafeParse(s"$a = $aVal in $b = $bVal in $body") shouldBe
              AST.Let(a.toLowerCase, unsafeParse(aVal), AST.Let(b.toLowerCase, unsafeParse(bVal), unsafeParse(body)))
          }
        }
      }

      "a * b + c = (a * b) + c" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a * $b + $c") shouldBe AST.App2(
            AST.Const(Constant2.Add),
            AST.App2(AST.Const(Constant2.Multiply), unsafeParse(a), unsafeParse(b)),
            unsafeParse(c)
          )
        }
      }

      "a + b * c = a + (b * c)" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a + $b * $c") shouldBe AST.App2(
            AST.Const(Constant2.Add),
            unsafeParse(a),
            AST.App2(AST.Const(Constant2.Multiply), unsafeParse(b), unsafeParse(c))
          )
        }
      }

      "(a + b) * c" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"($a + $b) * $c") shouldBe AST.App2(
            AST.Const(Constant2.Multiply),
            AST.App2(AST.Const(Constant2.Add), unsafeParse(a), unsafeParse(b)),
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
            AST.App2(AST.Const(Constant2.Add), unsafeParse(expr1), unsafeParse(expr2))
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
          unsafeParse(s"$identifier1($expr1, $expr2)") shouldBe AST.App2(
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
          unsafeParse("2^3 + 1") shouldBe AST.App2(
            AST.Const(Constant2.Add),
            AST.App2(AST.Const(Constant2.Exp), AST.Number("2"), AST.Number("3")),
            AST.Number("1")
          )
        }

        "2^3 * 2" in {
          unsafeParse("2^3 * 2") shouldBe AST.App2(
            AST.Const(Constant2.Multiply),
            AST.App2(AST.Const(Constant2.Exp), AST.Number("2"), AST.Number("3")),
            AST.Number("2")
          )
        }

        "2 * 2^3" in {
          unsafeParse("2 * 2^3") shouldBe AST.App2(
            AST.Const(Constant2.Multiply),
            AST.Number("2"),
            AST.App2(AST.Const(Constant2.Exp), AST.Number("2"), AST.Number("3"))
          )
        }
      }

      "divisions" - {
        "a / b + c = (a / b) + c" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"$a / $b + $c") shouldBe AST.App2(
              AST.Const(Constant2.Add),
              AST.App2(AST.Const(Constant2.Div), unsafeParse(a), unsafeParse(b)),
              unsafeParse(c)
            )
          }
        }

        "a + b / c = a + (b / c)" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"$a + $b / $c") shouldBe AST.App2(
              AST.Const(Constant2.Add),
              unsafeParse(a),
              AST.App2(AST.Const(Constant2.Div), unsafeParse(b), unsafeParse(c))
            )
          }
        }

        "(a + b) / c" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"($a + $b) / $c") shouldBe AST.App2(
              AST.Const(Constant2.Div),
              AST.App2(AST.Const(Constant2.Add), unsafeParse(a), unsafeParse(b)),
              unsafeParse(c)
            )
          }
        }

        "@point(a, b)" in {
          forAll(genLeafExpr, genLeafExpr) { (a, b) =>
            unsafeParse(s"@point($a, $b)") shouldBe AST.App2(
              AST.Const(Constant2.LiftedPoint),
              unsafeParse(a),
              unsafeParse(b))
          }
        }

        "@(expr(a, b))" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (expr, a, b) =>
            unsafeParse(s"@($expr($a, $b))") shouldBe AST.App(
              AST.Const(Constant1.Constant),
              AST.App2(unsafeParse(expr), unsafeParse(a), unsafeParse(b)))
          }
        }

        "@n" in {
          forAll(arbitrary[Double]) { d =>
            unsafeParse(s"@($d)") shouldBe
              AST.App(AST.Const(Constant1.Constant), unsafeParse(d.toString))
          }
        }

        "[a, b, c]" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"[$a, $b, $c]") shouldBe AST.App2(
              AST.Const(Constant2.Cons),
              unsafeParse(a),
              AST.App2(
                AST.Const(Constant2.Cons),
                unsafeParse(b),
                AST.App2(AST.Const(Constant2.Cons), unsafeParse(c), AST.Const(Constant0.Empty)))
            )
          }
        }
      }

      "zipWith(a, b, c, f)" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c, f) =>
          val parsed = unsafeParse(s"zipWith($a, $b, $c, $f)")
          val expected = AST.App3(
            AST.Const(Constant3.ZipWith),
            AST.App3(
              AST.Const(Constant3.ZipWith),
              unsafeParse(a),
              unsafeParse(b),
              unsafeParse(f)
            ),
            unsafeParse(c),
            AST.Lambda(
              "argf",
              AST.Lambda(
                "arg3",
                AST.App(AST.Identifier("argf"), AST.Identifier("arg3"))
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
            AST.App2(
              AST.Const(Constant2.Or),
              unsafeParse(a),
              AST.App2(AST.Const(Constant2.And), unsafeParse(b), unsafeParse(c)))
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
