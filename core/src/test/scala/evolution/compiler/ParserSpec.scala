package evolution.compiler
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{ Gen, Shrink }
import evolution.compiler.tree.TreeF._
import evolution.compiler.phases.parsing.Parser
import evolution.compiler.phases.typing.config.{ Constant0, Constant1, Constant2, Constant3 }
import evolution.compiler.tree.{ SpecialSyntax, Tree }

class ParserSpec extends LanguageSpec {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  "The expression parser" - {
    "should parse" - {
      "int literals" in {
        forAll { n: Int =>
          unsafeParse(n.toString) shouldEq IntLiteral(n).embed
        }
      }

      "doubles literals that are not integers" in {
        forAll { d: Double =>
          whenever(d % 1 != 0) {
            unsafeParse(d.toString) shouldEq DoubleLiteral(d).embed
          }
        }
      }

      "variables" in {
        forAll(genIdentifier) { varName =>
          unsafeParse(s"$varName") shouldEq Identifier(varName).embed
        }
      }

      "parse binary operators" in {
        forAll(genLeafExpr, genOperatorWithTree, genLeafExpr) {
          case (a, (op, opFunc), b) =>
            unsafeParse(s"$a $op $b") shouldEq opFunc(unsafeParse(a), unsafeParse(b))
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
              unsafeParse(left) shouldEq unsafeParse(right)
            }
        }
      }

      "inverses" in {
        unsafeParse("-point(0, 0)") shouldEq App
          .of(
            Identifier.const(Constant1.Inverse).embed,
            App.of(Identifier.const(Constant2.Point).embed, IntLiteral(0).embed, IntLiteral(0).embed).embed
          )
          .embed
      }

      "bindings" - {
        "a = 2 in $a" in {
          forAll(genIdentifier, genLeafExpr) { (id, expr) =>
            unsafeParse(s"$id = $expr in $id") shouldEq Let(id.toLowerCase, unsafeParse(expr), Identifier(id).embed).embed
          }
        }

        "a = b in\\n 1 + 2" in {
          forAll(genIdentifier, genLeafExpr) { (id, expr) =>
            unsafeParse(s"$id = $expr in 1 + 2") shouldEq Let(
              id.toLowerCase,
              unsafeParse(expr),
              App.of(Identifier.const(Constant2.Add).embed, IntLiteral(1).embed, IntLiteral(2).embed).embed
            ).embed
          }
        }

        "a = aval in b = bval in body" in {
          forAll(genIdentifier, genLeafExpr, genIdentifier, genLeafExpr, genLeafExpr) { (a, aVal, b, bVal, body) =>
            unsafeParse(s"$a = $aVal in $b = $bVal in $body") shouldEq
              Let(a.toLowerCase, unsafeParse(aVal), Let(b.toLowerCase, unsafeParse(bVal), unsafeParse(body)).embed).embed
          }
        }

        "f(x) = y in body" in {
          forAll(genIdentifier, genIdentifier, genLeafExpr, genLeafExpr) { (f, x, y, body) =>
            unsafeParse(s"$f($x) = $y in $body") shouldEq unsafeParse(s"$f = $x -> $y in $body")
          }
        }

        "f(x, y) = z in body" in {
          forAll(genIdentifier, genIdentifier, genIdentifier, genLeafExpr, genLeafExpr) { (f, x, y, z, body) =>
            unsafeParse(s"$f($x, $y) = $z in $body") shouldEq unsafeParse(s"$f = $x -> $y -> $z in $body")
          }
        }
      }

      "sampling" - {
        "a <- b in a" in {
          unsafeParse("a <- b in c") shouldEq
            App
              .of(
                Identifier.const(Constant2.WithFirst).embed,
                Identifier("b").embed,
                Lambda("a", Identifier("c").embed).embed
              )
              .embed
        }
      }

      "a * b + c = (a * b) + c" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a * $b + $c") shouldEq App
            .of(
              Identifier.const(Constant2.Add).embed,
              App.of(Identifier.const(Constant2.Multiply).embed, unsafeParse(a), unsafeParse(b)).embed,
              unsafeParse(c)
            )
            .embed
        }
      }

      "a + b * c = a + (b * c)" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a + $b * $c") shouldEq App
            .of(
              Identifier.const(Constant2.Add).embed,
              unsafeParse(a),
              App.of(Identifier.const(Constant2.Multiply).embed, unsafeParse(b), unsafeParse(c)).embed
            )
            .embed
        }
      }

      "(a + b) * c" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"($a + $b) * $c") shouldEq App
            .of(
              Identifier.const(Constant2.Multiply).embed,
              App.of(Identifier.const(Constant2.Add).embed, unsafeParse(a), unsafeParse(b)).embed,
              unsafeParse(c)
            )
            .embed
        }
      }

      "lambdas" in {
        forAll(genIdentifier, genLeafExpr) { (identifier, expr) =>
          unsafeParse(s"$identifier -> $expr") shouldEq Lambda(identifier.toLowerCase, unsafeParse(expr)).embed
        }
      }

      "HO lambdas" in {
        forAll(genIdentifier, genIdentifier, genLeafExpr) { (identifier1, identifier2, expr) =>
          unsafeParse(s"$identifier1 -> $identifier2 ->$expr") shouldEq Lambda(
            identifier1.toLowerCase,
            Lambda(identifier2.toLowerCase, unsafeParse(expr)).embed
          ).embed
        }
      }

      "Let bindings" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (id, value, in) =>
          unsafeParse(s"$id = $value in $in") shouldEq Let(id.toLowerCase, unsafeParse(value), unsafeParse(in)).embed
        }
      }

      "comparisons" - {
        "a < b" in {
          forAll(genLeafExpr, genLeafExpr) { (a, b) =>
            val parsed = unsafeParse(s"$a < $b")
            val expected = App.of(Identifier.const(Constant2.LessThan).embed, unsafeParse(a), unsafeParse(b)).embed
            parsed shouldEq expected
          }
        }
      }

      "a -> b + c = a -> (b + c)" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (identifier1, expr1, expr2) =>
          unsafeParse(s"$identifier1 -> $expr1 + $expr2") shouldEq Lambda(
            identifier1.toLowerCase,
            App.of(Identifier.const(Constant2.Add).embed, unsafeParse(expr1), unsafeParse(expr2)).embed
          ).embed
        }
      }

      "expressions with whitespaces at the beginning and at the end" in {
        forAll(genWhitespace, genLeafExpr, genWhitespace) { (wsStart, expr, wsEnd) =>
          unsafeParse(s"$wsStart$expr$wsEnd") shouldEq unsafeParse(expr)
        }
      }

      "ignore comments" in {
        forAll(genLeafExpr, Gen.alphaNumStr, Gen.alphaNumStr) { (expr, comment1, comment2) =>
          unsafeParse(s"//x$comment1\n$expr\n//y$comment2") shouldEq unsafeParse(expr)
        }
      }

      "parse applications of vars" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (identifier1, expr1, expr2) =>
          unsafeParse(s"$identifier1($expr1, $expr2)") shouldEq App
            .of(
              Identifier(identifier1).embed,
              unsafeParse(expr1),
              unsafeParse(expr2)
            )
            .embed
        }
      }

      "parse applications with pipe syntax" in {
        forAll(genIdentifier, genLeafExpr) { (identifier1, expr1) =>
          unsafeParse(s"$expr1 >> $identifier1") shouldEq App
            .of(
              Identifier(identifier1).embed,
              unsafeParse(expr1)
            )
            .embed
        }
      }

      "parse applications with dot syntax and single arguments" in {
        forAll(genIdentifier, genIdentifier) { (identifier1, expr1) =>
          unsafeParse(s"$expr1.$identifier1") shouldEq App
            .of(
              Identifier(identifier1).embed,
              unsafeParse(expr1)
            )
            .embed
        }
      }

      "parse applications with dot syntax and multiple arguments" in {
        forAll(genIdentifier, genIdentifier, genLeafExpr) { (identifier1, expr1, expr2) =>
          unsafeParse(s"$expr1.$identifier1($expr2)") shouldEq App
            .of(
              Identifier(identifier1).embed,
              unsafeParse(expr1),
              unsafeParse(expr2)
            )
            .embed
        }
      }

      "parse applications with dot syntax where receiver is an application" in {
        unsafeParse(s"a(b).method(c)") shouldEq App
          .of(
            Identifier("method").embed,
            App.of(Identifier("a").embed, Identifier("b").embed).embed,
            Identifier("c").embed
          )
          .embed
      }

      "parse chained dot selections" in {
        unsafeParse(s"a(b).method1(c).method2(d)") shouldEq App
          .of(
            Identifier("method2").embed,
            App
              .of(
                Identifier("method1").embed,
                App.of(Identifier("a").embed, Identifier("b").embed).embed,
                Identifier("c").embed
              )
              .embed,
            Identifier("d").embed
          )
          .embed
      }

      "parse applications of lambdas" in {
        forAll(genLambda, genLeafExpr) { (lambda, expr) =>
          unsafeParse(s"($lambda)($expr)") shouldEq App
            .of(
              unsafeParse(lambda),
              unsafeParse(expr)
            )
            .embed
        }
      }

      "parse exponentials" - {
        "2^3 + 1" in {
          unsafeParse("2^3 + 1") shouldEq App
            .of(
              Identifier.const(Constant2.Add).embed,
              App.of(Identifier.const(Constant2.Exp).embed, IntLiteral(2).embed, IntLiteral(3).embed).embed,
              IntLiteral(1).embed
            )
            .embed
        }

        "2^3 * 2" in {
          unsafeParse("2^3 * 2") shouldEq App
            .of(
              Identifier.const(Constant2.Multiply).embed,
              App.of(Identifier.const(Constant2.Exp).embed, IntLiteral(2).embed, IntLiteral(3).embed).embed,
              IntLiteral(2).embed
            )
            .embed
        }

        "2 * 2^3" in {
          unsafeParse("2 * 2^3") shouldEq App
            .of(
              Identifier.const(Constant2.Multiply).embed,
              IntLiteral(2).embed,
              App.of(Identifier.const(Constant2.Exp).embed, IntLiteral(2).embed, IntLiteral(3).embed).embed
            )
            .embed
        }
      }

      "divisions" - {
        "a / b + c = (a / b) + c" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"$a / $b + $c") shouldEq App
              .of(
                Identifier.const(Constant2.Add).embed,
                App.of(Identifier.const(Constant2.Div).embed, unsafeParse(a), unsafeParse(b)).embed,
                unsafeParse(c)
              )
              .embed
          }
        }

        "a + b / c = a + (b / c)" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"$a + $b / $c") shouldEq App
              .of(
                Identifier.const(Constant2.Add).embed,
                unsafeParse(a),
                App.of(Identifier.const(Constant2.Div).embed, unsafeParse(b), unsafeParse(c)).embed
              )
              .embed
          }
        }

        "(a + b) / c" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"($a + $b) / $c") shouldEq App
              .of(
                Identifier.const(Constant2.Div).embed,
                App.of(Identifier.const(Constant2.Add).embed, unsafeParse(a), unsafeParse(b)).embed,
                unsafeParse(c)
              )
              .embed
          }
        }

        "@point(a, b)" in {
          forAll(genLeafExpr, genLeafExpr) { (a, b) =>
            unsafeParse(s"@point($a, $b)") shouldEq App
              .of(
                Identifier.const(Constant2.LiftedPoint).embed,
                unsafeParse(a),
                unsafeParse(b)
              )
              .embed
          }
        }

        "const(expr(a, b))" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (expr, a, b) =>
            unsafeParse(s"const($expr($a, $b))") shouldEq App
              .of(
                Identifier.const(Constant1.Constant).embed,
                App.of(unsafeParse(expr), unsafeParse(a), unsafeParse(b)).embed
              )
              .embed
          }
        }

        "const(n)" in {
          forAll(arbitrary[Double]) { d =>
            unsafeParse(s"const($d)") shouldEq
              App.of(Identifier.const(Constant1.Constant).embed, unsafeParse(d.toString)).embed
          }
        }

        "[a, b, c]" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"[$a, $b, $c]") shouldEq App
              .of(
                Identifier.const(Constant2.Cons).embed,
                unsafeParse(a),
                App
                  .of(
                    Identifier.const(Constant2.Cons).embed,
                    unsafeParse(b),
                    App
                      .of(
                        Identifier.const(Constant2.Cons).embed,
                        unsafeParse(c),
                        Identifier.const(Constant0.Empty).embed
                      )
                      .embed
                  )
                  .embed
              )
              .embed
          }
        }
      }

      "variadic zipWith(a, b, c, f)" in {
        pending
        forAll(genLeafExpr, genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c, f) =>
          val parsed = unsafeParse(s"zipWith($a, $b, $c, $f)")
          val expected = App
            .of(
              Identifier.const(Constant3.ZipWith).embed,
              App
                .of(
                  Identifier.const(Constant3.ZipWith).embed,
                  unsafeParse(a),
                  unsafeParse(b),
                  unsafeParse(f)
                )
                .embed,
              unsafeParse(c),
              Lambda(
                "f",
                Lambda(
                  "x",
                  App.of(Identifier("f").embed, Identifier("x").embed).embed
                ).embed
              ).embed
            )
            .embed

          parsed shouldEq expected
        }
      }

      "zip(a <- as, b <- bs, c <- cs) in d" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c, d) =>
          val parsed = unsafeParse(s"zip(a <- $a, b <- $b, c <- $c) in $d")
          val lambda = Lambda("a", Lambda("b", Lambda("c", unsafeParse(d)).embed).embed).embed
          val expected = App
            .of(
              Identifier.const(Constant3.ZipWith).embed,
              App
                .of(
                  Identifier.const(Constant3.ZipWith).embed,
                  unsafeParse(a),
                  unsafeParse(b),
                  lambda
                )
                .embed,
              unsafeParse(c),
              Lambda(
                "f",
                Lambda(
                  "x",
                  App.of(Identifier("f").embed, Identifier("x").embed).embed
                ).embed
              ).embed
            )
            .embed

          parsed shouldEq expected
        }
      }

      "product(a <- as, b <- bs) in d" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          val parsed = unsafeParse(s"product(a <- $a, b <- $b) in $c")
          val expected = SpecialSyntax.product(List("a" -> unsafeParse(a), "b" -> unsafeParse(b)), unsafeParse(c))

          parsed shouldEq expected
        }
      }

      "uniformChoice(a, b, c)" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          val parsed = unsafeParse(s"uniformChoice($a, $b, $c)")
          val expected = SpecialSyntax.uniformChoice(List(unsafeParse(a), unsafeParse(b), unsafeParse(c)))
          parsed shouldEq expected
        }
      }

      "boolean literals" in {
        unsafeParse("true") shouldEq Bool(true).embed
        unsafeParse("false") shouldEq Bool(false).embed
      }

      "not" in {
        forAll(genLeafExpr) { a =>
          unsafeParse(s"!$a") shouldEq
            App.of(Identifier.const(Constant1.Not).embed, unsafeParse(a)).embed
        }
      }

      "logical operators with the right precedence" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a || $b && $c") shouldEq
            App
              .of(
                Identifier.const(Constant2.Or).embed,
                unsafeParse(a),
                App.of(Identifier.const(Constant2.And).embed, unsafeParse(b), unsafeParse(c)).embed
              )
              .embed
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
        lineNumber shouldEq Left(1)
      }
    }
  }

  def unsafeParse(string: String): Tree = Parser.parse(string).toTry.get
}
