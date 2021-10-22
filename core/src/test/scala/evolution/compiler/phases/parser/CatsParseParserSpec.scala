package evolution.compiler.phases.parser

import evolution.compiler.LanguageSpec
import evolution.compiler.tree.Tree._
import evolution.compiler.tree.{SpecialSyntax, Tree}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Gen, Shrink}

class CatsParseParserSpec extends LanguageSpec {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  "The expression parser" - {
    "should parse" - {
      "int literals" in {
        forAll { n: Int =>
          unsafeParse(s"   $n   ") shouldEq IntLiteral(n)
        }
      }

      "doubles literals that are not integers" in {
        forAll { d: Double =>
          whenever(d % 1 != 0) {

            unsafeParse("1.1") shouldEq DoubleLiteral(1.1)
            unsafeParse(d.toString) shouldEq DoubleLiteral(d)
          }
        }
      }

      "variables" in {
        forAll(genIdentifier) { varName =>
          unsafeParse(s"$varName") shouldEq Id(varName)
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
            Id("inverse"),
            App.of(Id("point"), IntLiteral(0), IntLiteral(0))
          )

      }

      "bindings" - {
        "a = 2 in $a" in {
          forAll(genIdentifier, genLeafExpr) { (id, expr) =>
            unsafeParse(s"$id = $expr in $id") shouldEq Let(id.toLowerCase, unsafeParse(expr), Id(id))
          }
        }

        "a = b in\\n 1 + 2" in {
          forAll(genIdentifier, genLeafExpr) { (id, expr) =>
            unsafeParse(s"$id = $expr in 1 + 2") shouldEq Let(
              id.toLowerCase,
              unsafeParse(expr),
              App.of(Id("add"), IntLiteral(1), IntLiteral(2))
            )
          }
        }

        "a = aval in b = bval in body" in {
          forAll(genIdentifier, genLeafExpr, genIdentifier, genLeafExpr, genLeafExpr) { (a, aVal, b, bVal, body) =>
            unsafeParse(s"$a = $aVal in $b = $bVal in $body") shouldEq
              Let(a.toLowerCase, unsafeParse(aVal), Let(b.toLowerCase, unsafeParse(bVal), unsafeParse(body)))
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
                Id("withfirst"),
                Id("b"),
                Lambda("a", Id("c"))
              )

        }
      }

      "a * b + c = (a * b) + c" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a * $b + $c") shouldEq App
            .of(
              Id("add"),
              App.of(Id("multiply"), unsafeParse(a), unsafeParse(b)),
              unsafeParse(c)
            )

        }
      }

      "a + b * c = a + (b * c)" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"$a + $b * $c") shouldEq App
            .of(
              Id("add"),
              unsafeParse(a),
              App.of(Id("multiply"), unsafeParse(b), unsafeParse(c))
            )

        }
      }

      "(a + b) * c" in {
        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
          unsafeParse(s"($a + $b) * $c") shouldEq App
            .of(
              Id("multiply"),
              App.of(Id("add"), unsafeParse(a), unsafeParse(b)),
              unsafeParse(c)
            )

        }
      }

      "lambdas" in {
        forAll(genIdentifier, genLeafExpr) { (identifier, expr) =>
          unsafeParse(s"$identifier -> $expr") shouldEq Lambda(identifier.toLowerCase, unsafeParse(expr))
        }
      }

      "HO lambdas" in {
        forAll(genIdentifier, genIdentifier, genLeafExpr) { (identifier1, identifier2, expr) =>
          unsafeParse(s"$identifier1 -> $identifier2 ->$expr") shouldEq Lambda(
            identifier1.toLowerCase,
            Lambda(identifier2.toLowerCase, unsafeParse(expr))
          )
        }
      }

      "Let bindings" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (id, value, in) =>
          unsafeParse(s"$id = $value in $in") shouldEq Let(id.toLowerCase, unsafeParse(value), unsafeParse(in))
        }
      }

      "comparisons" - {
        "a < b" in {
          forAll(genLeafExpr, genLeafExpr) { (a, b) =>
            val parsed = unsafeParse(s"$a < $b")
            val expected = App.of(Id("lessthan"), unsafeParse(a), unsafeParse(b))
            parsed shouldEq expected
          }
        }
      }

      "a -> b + c = a -> (b + c)" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (identifier1, expr1, expr2) =>
          unsafeParse(s"$identifier1 -> $expr1 + $expr2") shouldEq Lambda(
            identifier1.toLowerCase,
            App.of(Id("add"), unsafeParse(expr1), unsafeParse(expr2))
          )
        }
      }

      "expressions with whitespaces at the beginning and at the end" in {
        forAll(genWhitespace, genLeafExpr, genWhitespace) { (wsStart, expr, wsEnd) =>
          unsafeParse(s"$wsStart$expr$wsEnd") shouldEq unsafeParse(expr)
        }
      }
//
//      "ignore comments" in {
//        forAll(genLeafExpr, Gen.alphaNumStr, Gen.alphaNumStr) { (expr, comment1, comment2) =>
//          unsafeParse(s"//x$comment1\n$expr\n//y$comment2") shouldEq unsafeParse(expr)
//        }
//      }
//
      "parse applications of vars" in {
        forAll(genIdentifier, genLeafExpr, genLeafExpr) { (identifier1, expr1, expr2) =>
          unsafeParse(s"$identifier1($expr1, $expr2)") shouldEq App
            .of(
              Id(identifier1),
              unsafeParse(expr1),
              unsafeParse(expr2)
            )

        }
      }
//
//      "parse applications with pipe syntax" in {
//        forAll(genIdentifier, genLeafExpr) { (identifier1, expr1) =>
//          unsafeParse(s"$expr1 >> $identifier1") shouldEq App
//            .of(
//              Id(identifier1),
//              unsafeParse(expr1)
//            )
//
//        }
//      }
//
//      "parse applications with dot syntax and no arguments" in {
//        forAll(genIdentifier, genIdentifier) { (identifier1, expr1) =>
//          unsafeParse(s"$expr1.$identifier1") shouldEq App
//            .of(
//              Id(identifier1),
//              unsafeParse(expr1)
//            )
//
//        }
//      }
//
//      "parse applications with dot syntax and single argument" in {
//        forAll(genIdentifier, genIdentifier, genLeafExpr) { (identifier1, expr1, expr2) =>
//          unsafeParse(s"$expr1.$identifier1($expr2)") shouldEq App
//            .of(
//              Id(identifier1),
//              unsafeParse(expr1),
//              unsafeParse(expr2)
//            )
//
//        }
//      }
//
//      "parse applications with dot syntax and multiple arguments" in {
//        unsafeParse(s"a.method(b, c)") shouldEq App
//          .of(
//            Id("method"),
//            Id("a"),
//            Id("b"),
//            Id("c")
//          )
//
//      }
//
//      "parse applications with dot syntax where receiver is an application" in {
//        unsafeParse(s"a(b).method(c)") shouldEq App
//          .of(
//            Id("method"),
//            App.of(Id("a"), Id("b")),
//            Id("c")
//          )
//
//      }
//
      "parse chained dot selections" in {
        unsafeParse(s"a(b).method1(c).method2(d)") shouldEq App
          .of(
            Id("method2"),
            App
              .of(
                Id("method1"),
                App.of(Id("a"), Id("b")),
                Id("c")
              ),
            Id("d")
          )

      }
//
//      "parse applications of lambdas" in {
//        forAll(genLambda, genLeafExpr) { (lambda, expr) =>
//          unsafeParse(s"($lambda)($expr)") shouldEq App
//            .of(
//              unsafeParse(lambda),
//              unsafeParse(expr)
//            )
//
//        }
//      }

      "parse exponentials" - {
        "2^3 + 1" in {
          unsafeParse("2^3 + 1") shouldEq App
            .of(
              Id("add"),
              App.of(Id("exp"), IntLiteral(2), IntLiteral(3)),
              IntLiteral(1)
            )

        }

        "2^3 * 2" in {
          unsafeParse("2^3 * 2") shouldEq App
            .of(
              Id("multiply"),
              App.of(Id("exp"), IntLiteral(2), IntLiteral(3)),
              IntLiteral(2)
            )

        }

        "2 * 2^3" in {
          unsafeParse("2 * 2^3") shouldEq App
            .of(
              Id("multiply"),
              IntLiteral(2),
              App.of(Id("exp"), IntLiteral(2), IntLiteral(3))
            )

        }
      }
//
      "divisions" - {
        "a / b + c = (a / b) + c" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"$a / $b + $c") shouldEq App
              .of(
                Id("add"),
                App.of(Id("div"), unsafeParse(a), unsafeParse(b)),
                unsafeParse(c)
              )

          }
        }

        "a + b / c = a + (b / c)" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"$a + $b / $c") shouldEq App
              .of(
                Id("add"),
                unsafeParse(a),
                App.of(Id("div"), unsafeParse(b), unsafeParse(c))
              )

          }
        }

        "(a + b) / c" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"($a + $b) / $c") shouldEq App
              .of(
                Id("div"),
                App.of(Id("add"), unsafeParse(a), unsafeParse(b)),
                unsafeParse(c)
              )

          }
        }

        "@point(a, b)" in {
          forAll(genLeafExpr, genLeafExpr) { (a, b) =>
            unsafeParse(s"@point($a, $b)") shouldEq App
              .of(
                Id("@point"),
                unsafeParse(a),
                unsafeParse(b)
              )

          }
        }

        "const(expr(a, b))" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (expr, a, b) =>
            unsafeParse(s"const($expr($a, $b))") shouldEq App
              .of(
                Id("const"),
                App.of(unsafeParse(expr), unsafeParse(a), unsafeParse(b))
              )

          }
        }

        "const(n)" in {
          forAll(arbitrary[Double]) { d =>
            unsafeParse(s"const($d)") shouldEq
              App.of(Id("const"), unsafeParse(d.toString))
          }
        }

        "[a, b, c]" in {
          forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
            unsafeParse(s"[$a, $b, $c]") shouldEq App
              .of(
                Id("cons"),
                unsafeParse(a),
                App
                  .of(
                    Id("cons"),
                    unsafeParse(b),
                    App
                      .of(
                        Id("cons"),
                        unsafeParse(c),
                        Id("empty")
                      )
                  )
              )

          }
        }
      }
//
//      "variadic zipWith(a, b, c, f)" in {
//        pending
//        forAll(genLeafExpr, genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c, f) =>
//          val parsed = unsafeParse(s"zipWith($a, $b, $c, $f)")
//          val expected = App
//            .of(
//              Id("zipwith"),
//              App
//                .of(
//                  Id("zipwith"),
//                  unsafeParse(a),
//                  unsafeParse(b),
//                  unsafeParse(f)
//                ),
//              unsafeParse(c),
//              Lambda(
//                "f",
//                Lambda(
//                  "x",
//                  App.of(Id("f"), Id("x"))
//                )
//              )
//            )
//
//          parsed shouldEq expected
//        }
//      }
//
//      "zip(a <- as, b <- bs, c <- cs) in d" in {
//        forAll(genLeafExpr, genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c, d) =>
//          val parsed = unsafeParse(s"zip(a <- $a, b <- $b, c <- $c) in $d")
//          val lambda = Lambda("a", Lambda("b", Lambda("c", unsafeParse(d))))
//          val expected = App
//            .of(
//              Id("zipwith"),
//              App
//                .of(
//                  Id("zipwith"),
//                  unsafeParse(a),
//                  unsafeParse(b),
//                  lambda
//                ),
//              unsafeParse(c),
//              Lambda(
//                "f",
//                Lambda(
//                  "x",
//                  App.of(Id("f"), Id("x"))
//                )
//              )
//            )
//
//          parsed shouldEq expected
//        }
//      }
//
//      "product(a <- as, b <- bs) in d" in {
//        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
//          val parsed = unsafeParse(s"product(a <- $a, b <- $b) in $c")
//          val expected = SpecialSyntax.product(List("a" -> unsafeParse(a), "b" -> unsafeParse(b)), unsafeParse(c))
//
//          parsed shouldEq expected
//        }
//      }
//
//      "uniformChoice(a, b, c)" in {
//        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
//          val parsed = unsafeParse(s"uniformChoice($a, $b, $c)")
//          val expected = SpecialSyntax.uniformChoice(List(unsafeParse(a), unsafeParse(b), unsafeParse(c)))
//          parsed shouldEq expected
//        }
//      }
//
//      "boolean literals" in {
//        unsafeParse("true") shouldEq Bool(true)
//        unsafeParse("false") shouldEq Bool(false)
//      }
//
//      "not" in {
//        forAll(genLeafExpr) { a =>
//          unsafeParse(s"!$a") shouldEq
//            App.of(Id("not"), unsafeParse(a))
//        }
//      }
//
//      "logical operators with the right precedence" in {
//        forAll(genLeafExpr, genLeafExpr, genLeafExpr) { (a, b, c) =>
//          unsafeParse(s"$a || $b && $c") shouldEq
//            App
//              .of(
//                Id("or"),
//                unsafeParse(a),
//                App.of(Id("and"), unsafeParse(b), unsafeParse(c))
//              )
//
//        }
//      }
    }

//    "when it fails" - {
//      "it should report the line number" in {
//        val expr = """|a    = 10 in
//                      |g(a a)
//                   """.stripMargin
//
//        val failure = CatsParseParser.parse(expr)
//        // The first line is 0
//        val lineNumber = failure.left.map(_.lineNumber)
//        lineNumber shouldEq Left(1)
//      }
//    }
  }

  def unsafeParse(string: String): Tree = CatsParseParser.parse(string).toTry.get
}
