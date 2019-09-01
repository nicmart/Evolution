package evolution.compilertree
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{ Gen, Shrink }
import evolution.compilertree.ast.TreeF._
import evolution.compilertree.phases.parsing.Parser
import evolution.compilertree.phases.typing.config.{ Constant0, Constant1, Constant2, Constant3 }
import evolution.compilertree.ast.SpecialSyntax

class ParserSpec extends LanguageSpec {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  "The expression parser" - {
    "should parse" - {
      "int literals" in {
        forAll { n: Int =>
          unsafeParse(n.toString) === IntLiteral(n).embed
        }
      }

      "doubles literals that are not integers" in {
        forAll { d: Double =>
          whenever(d % 1 != 0) {
            unsafeParse(d.toString) === DoubleLiteral(d).embed
          }
        }
      }

      "variables" in {
        forAll(genIdentifier) { varName =>
          unsafeParse(s"$varName") === Identifier(varName).embed
        }
      }

      "parse binary operators" in {
        forAll(genLeafExpr, genOperatorWithTree, genLeafExpr) {
          case (a, (op, opAST), b) =>
            unsafeParse(s"$a $op $b") === AppN(opAST, unsafeParse(a), unsafeParse(b))
        }
      }

      "associate left to right for ops with the same precedence" - {
        val expectations = List(
          "a - b + c" -> "(a - b) + c",
          "a + b - c" -> "(a + b) - c",
          "a - b - c - d" -> "((a - b) - c) - d"
        )
        expectations.foreach {
          case (left, right) =>
            s"$left = $right" in {
              unsafeParse(left) === unsafeParse(right)
            }
        }
      }

      "inverses" in {
        unsafeParse("-point(0, 0)") === App(
          Const(Constant1.Inverse),
          AppN(Const(Constant2.Point), IntLiteral(0).embed, IntLiteral(0).embed)
        ).embed
      }

      "bindings" - {
        "a = 2 in $a" in {
          forAll(genIdentifier, genLeafExpr) { (id, expr) =>
            unsafeParse(s"$id =$expr in $id") === Let(id.toLowerCase, unsafeParse(expr), Identifier(id).embed).embed
          }
        }

        "a = b in\\n 1 + 2" in {
          forAll(genIdentifier, genLeafExpr) { (id, expr) =>
            unsafeParse(s"$id = $expr in 1 + 2") === Let(
              id.toLowerCase,
              unsafeParse(expr),
              AppN(Const(Constant2.Add), IntLiteral(1).embed, IntLiteral(2).embed)
            ).embed
          }
        }

        "a = aval in b = bval in body" in {
          forAll(genIdentifier, genLeafExpr, genIdentifier, genLeafExpr, genLeafExpr) { (a, aVal, b, bVal, body) =>
            unsafeParse(s"$a = $aVal in $b = $bVal in $body") ===
              Let(a.toLowerCase, unsafeParse(aVal), Let(b.toLowerCase, unsafeParse(bVal), unsafeParse(body)).embed).embed
          }
        }

        "f(x) = y in body" in {
          forAll(genIdentifier, genIdentifier, genLeafExpr, genLeafExpr) { (f, x, y, body) =>
            unsafeParse(s"$f($x) = $y in $body") === unsafeParse(s"$f = $x -> $y in $body")
          }
        }

        "f(x, y) = z in body" in {
          forAll(genIdentifier, genIdentifier, genIdentifier, genLeafExpr, genLeafExpr) { (f, x, y, z, body) =>
            unsafeParse(s"$f($x, $y) = $z in $body") === unsafeParse(s"$f = $x -> $y -> $z in $body")
          }
        }
      }

      "sampling" - {
        "a <- b in a" in {
          unsafeParse("a <- b in c") ===
            AppN(Const(Constant2.WithFirst), Identifier("b").embed, Lambda("a", Identifier("c").embed).embed)
        }
      }

      "a * b + c = (a * b) + c" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a * $b + $c") === AppN(
            Const(Constant2.Add),
            AppN(Const(Constant2.Multiply), unsafeParse(a), unsafeParse(b)),
            unsafeParse(c)
          )
        }
      }

      "a + b * c = a + (b * c)" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a + $b * $c") === AppN(
            Const(Constant2.Add),
            unsafeParse(a),
            AppN(Const(Constant2.Multiply), unsafeParse(b), unsafeParse(c))
          )
        }
      }

      "(a + b) * c" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"($a + $b) * $c") === AppN(
            Const(Constant2.Multiply),
            AppN(Const(Constant2.Add), unsafeParse(a), unsafeParse(b)),
            unsafeParse(c)
          )
        }
      }

      "lambdas" in {
        forAll(genIdentifier, genLeafExpr) { (identifier, expr) =>
          unsafeParse(s"$identifier -> $expr") === Lambda(identifier.toLowerCase, unsafeParse(expr)).embed
        }
      }

      "HO lambdas" in {
        forAll(genIdentifier, genIdentifier, genLeafExpr) { (identifier1, identifier2, expr) =>
          unsafeParse(s"$identifier1 -> $identifier2 ->$expr") === Lambda(
            identifier1.toLowerCase,
            Lambda(identifier2.toLowerCase, unsafeParse(expr)).embed
          ).embed
        }
      }

      "Let bindings" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (id, value, in) =>
          unsafeParse(s"$id = $value in $in") === Let(id.toLowerCase, unsafeParse(value), unsafeParse(in)).embed
        }
      }

      "comparisons" - {
        "a < b" in {
          forAll(genLeafExpr, genLeafExpr) { (a, b) =>
            val parsed = unsafeParse(s"$a < $b")
            val expected = AppN(Const(Constant2.LessThan), unsafeParse(a), unsafeParse(b))
            parsed === expected
          }
        }
      }

      "a -> b + c = a -> (b + c)" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (identifier1, expr1, expr2) =>
          unsafeParse(s"$identifier1 -> $expr1 + $expr2") === Lambda(
            identifier1.toLowerCase,
            AppN(Const(Constant2.Add), unsafeParse(expr1), unsafeParse(expr2))
          ).embed
        }
      }

      "expressions with whitespaces at the beginning and at the end" in {
        forAll(genWhitespace, genLeafExpr, genWhitespace) { (wsStart, expr, wsEnd) =>
          unsafeParse(s"$wsStart$expr$wsEnd") === unsafeParse(expr)
        }
      }

      "ignore comments" in {
        forAll(genLeafExpr, Gen.alphaNumStr, Gen.alphaNumStr) { (expr, comment1, comment2) =>
          unsafeParse(s"//$comment1\n$expr\n//$comment2") === unsafeParse(expr)
        }

      }

      "parse applications of vars" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (identifier1, expr1, expr2) =>
          unsafeParse(s"$identifier1($expr1, $expr2)") === AppN(
            Identifier(identifier1).embed,
            unsafeParse(expr1),
            unsafeParse(expr2)
          )
        }
      }

      "parse applications of lambdas" in {
        forAll(genLambda, genLeafExpr) { (lambda, expr) =>
          unsafeParse(s"($lambda)($expr)") === App(
            unsafeParse(lambda),
            unsafeParse(expr)
          ).embed
        }
      }

      "parse exponentials" - {
        "2^3 + 1" in {
          unsafeParse("2^3 + 1") === AppN(
            Const(Constant2.Add),
            AppN(Const(Constant2.Exp), IntLiteral(2).embed, IntLiteral(3).embed),
            IntLiteral(1).embed
          )
        }

        "2^3 * 2" in {
          unsafeParse("2^3 * 2") === AppN(
            Const(Constant2.Multiply),
            AppN(Const(Constant2.Exp), IntLiteral(2).embed, IntLiteral(3).embed),
            IntLiteral(2).embed
          )
        }

        "2 * 2^3" in {
          unsafeParse("2 * 2^3") === AppN(
            Const(Constant2.Multiply),
            IntLiteral(2).embed,
            AppN(Const(Constant2.Exp), IntLiteral(2).embed, IntLiteral(3).embed)
          )
        }
      }

      "divisions" - {
        "a / b + c = (a / b) + c" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"$a / $b + $c") === AppN(
              Const(Constant2.Add),
              AppN(Const(Constant2.Div), unsafeParse(a), unsafeParse(b)),
              unsafeParse(c)
            )
          }
        }

        "a + b / c = a + (b / c)" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"$a + $b / $c") === AppN(
              Const(Constant2.Add),
              unsafeParse(a),
              AppN(Const(Constant2.Div), unsafeParse(b), unsafeParse(c))
            )
          }
        }

        "(a + b) / c" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"($a + $b) / $c") === AppN(
              Const(Constant2.Div),
              AppN(Const(Constant2.Add), unsafeParse(a), unsafeParse(b)),
              unsafeParse(c)
            )
          }
        }

        "@point(a, b)" in {
          forAll(genLeafExpr, genLeafExpr) { (a, b) =>
            unsafeParse(s"@point($a, $b)") === AppN(
              Const(Constant2.LiftedPoint),
              unsafeParse(a),
              unsafeParse(b)
            )
          }
        }

        "const(expr(a, b))" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (expr, a, b) =>
            unsafeParse(s"const($expr($a, $b))") === App(
              Const(Constant1.Constant),
              AppN(unsafeParse(expr), unsafeParse(a), unsafeParse(b))
            ).embed
          }
        }

        "const(n)" in {
          forAll(arbitrary[Double]) { d =>
            unsafeParse(s"const($d)") ===
              App(Const(Constant1.Constant), unsafeParse(d.toString)).embed
          }
        }

        "[a, b, c]" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"[$a, $b, $c]") === AppN(
              Const(Constant2.Cons),
              unsafeParse(a),
              AppN(
                Const(Constant2.Cons),
                unsafeParse(b),
                AppN(Const(Constant2.Cons), unsafeParse(c), Const(Constant0.Empty))
              )
            )
          }
        }
      }

      "variadic zipWith(a, b, c, f)" in {
        pending
        forAll(genLeafExpr, genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c, f) =>
          val parsed = unsafeParse(s"zipWith($a, $b, $c, $f)")
          val expected = AppN(
            Const(Constant3.ZipWith),
            AppN(
              Const(Constant3.ZipWith),
              unsafeParse(a),
              unsafeParse(b),
              unsafeParse(f)
            ),
            unsafeParse(c),
            Lambda(
              "f",
              Lambda(
                "x",
                App(Identifier("f").embed, Identifier("x").embed).embed
              ).embed
            ).embed
          )

          parsed === expected
        }
      }

      "zip(a <- as, b <- bs, c <- cs) in d" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c, d) =>
          val parsed = unsafeParse(s"zip(a <- $a, b <- $b, c <- $c) in $d")
          val lambda = Lambda("a", Lambda("b", Lambda("c", unsafeParse(d)).embed).embed).embed
          val expected = AppN(
            Const(Constant3.ZipWith),
            AppN(
              Const(Constant3.ZipWith),
              unsafeParse(a),
              unsafeParse(b),
              lambda
            ),
            unsafeParse(c),
            Lambda(
              "f",
              Lambda(
                "x",
                App(Identifier("f").embed, Identifier("x").embed).embed
              ).embed
            ).embed
          )

          parsed === expected
        }
      }

      "product(a <- as, b <- bs) in d" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          val parsed = unsafeParse(s"product(a <- $a, b <- $b) in $c")
          val expected = SpecialSyntax.product(List("a" -> unsafeParse(a), "b" -> unsafeParse(b)), unsafeParse(c))

          parsed === expected
        }
      }

      "uniformChoice(a, b, c)" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          val parsed = unsafeParse(s"uniformChoice($a, $b, $c)")
          val expected = SpecialSyntax.uniformChoice(List(unsafeParse(a), unsafeParse(b), unsafeParse(c)))
          parsed === expected
        }
      }

      "boolean literals" in {
        unsafeParse("true") === Bool(true).embed
        unsafeParse("false") === Bool(false).embed
      }

      "not" in {
        forAll(genLeafExpr) { a =>
          unsafeParse(s"!$a") ===
            App(Const(Constant1.Not), unsafeParse(a)).embed
        }
      }

      "logical operators with the right precedence" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a || $b && $c") ===
            AppN(
              Const(Constant2.Or),
              unsafeParse(a),
              AppN(Const(Constant2.And), unsafeParse(b), unsafeParse(c))
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
        lineNumber === Left(1)
      }
    }
  }

  def unsafeParse(string: String): Tree = Parser.parse(string).toTry.get
}
