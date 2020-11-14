package evolution.compiler.phases.parser

import cats.data.NonEmptyList
import evolution.compiler.phases.{Parser, parser}
import evolution.compiler.phases.parser.ParserConfig.{whitespace, _}
import evolution.compiler.phases.parser.PrecedenceGroup.BinaryOperator
import evolution.compiler.tree
import evolution.compiler.tree.Tree
import evolution.compiler.tree.Tree._
import fastparse._

object FastParseParser extends Parser {
  def parse(astString: String): Either[ParserFailure, Tree] =
    fastparse
      .parse(astString, program(_))
      .fold((_, failIndex, extra) => Left(new ParserFailure(failIndex, extra)), (expr, _) => Right(expr))

  def binaryOperators: List[(String, BinaryOperator)] = allPrecedenceGroups.flatMap(group => group.operators)

  private def program[_: P]: P[Tree] =
    P(whitespaces ~ expression ~ whitespaces ~ End)

  private def expression[_: P]: P[Tree] =
    P(NonOperandExpressions.nonOperand | precedenceGroups.operand)

  private object NonOperandExpressions {
    def nonOperand[_: P]: P[Tree] = {
      // Note: a previous solution based on flatMap caused undesired back-tracking
      P(identifier ~ (lambdaTail | letTail | sampleTail | argsTail)).map {
        case (id, f) => f(id)
      }
    }

    private def lambdaTail[_: P]: P[String => Tree] = P(whitespaces ~ "->" ~/ expression).map { expr =>
      Lambda(_, expr)
    }

    // `a = 2 in` ... and `a == 2` share the same prefix, so the cut must be after the negative lookahead
    private def letTail[_: P]: P[String => Tree] =
      P(whitespaces ~ "=" ~ !"=" ~/ expression ~/ "in" ~/ expression).map {
        case (value, body) =>
          Let(_, value, body)
      }

    private def sampleTail[_: P]: P[String => Tree] =
      P(whitespaces ~ "<-" ~/ expression ~/ "in" ~/ expression).map {
        case (sampling, body) =>
          variable => tree.SpecialSyntax.withFirst(variable -> sampling, body)
      }

    // We need to allow backtracking, since f(x, y) can be a function application in addition to a binding
    private def argsTail[_: P]: P[String => Tree] =
      P(whitespaces ~~ "(" ~ NoCut(nonEmptyCsv(identifier)) ~ ")" ~ "=" ~ !"=" ~/ expression ~/ "in" ~/ expression)
        .map {
          case (args, value, body) => name => tree.SpecialSyntax.functionBinding(name, args.toList, value, body)
        }
  }

  private val allPrecedenceGroups = List(
    PrecedenceGroup(
      ">>" -> ((left, right) => App.of(right, left))
    ),
    PrecedenceGroup(
      "||" -> constOp("or")
    ),
    PrecedenceGroup(
      "&&" -> constOp("and")
    ),
    PrecedenceGroup(
      ">=" -> constOp("greaterthanorequal"),
      ">" -> constOp("greaterthan"),
      "<=" -> constOp("lessthanorequal"),
      "<" -> constOp("lessthan")
    ),
    PrecedenceGroup(
      "==" -> constOp("eq"),
      "!=" -> constOp("neq")
    ),
    PrecedenceGroup(
      "+" -> constOp("add"),
      "-" -> constOp("minus")
    ),
    PrecedenceGroup(
      "*" -> constOp("multiply"),
      "/" -> constOp("div"),
      "%" -> constOp("mod")
    ),
    PrecedenceGroup(
      "^" -> constOp("exp")
    )
  )

  private def constOp(name: String)(left: Tree, right: Tree): Tree =
    App.of(Id(name), left, right)

  // Operator groups, order by ascending Precedence
  private def precedenceGroups[_: P]: PrecedenceGroups = parser.PrecedenceGroups(
    () => atomicOperand,
    allPrecedenceGroups
  )

  private def factor[_: P]: P[Tree] = {
    def prefix: P[Tree] = P(("(" ~/ expression ~/ ")") | doubleLit | boolean | unaryPrefixOp | variable | list)

    def app: P[Tree] = P(prefix ~/ ("(" ~/ nonEmptyArgs ~/ ")").?).map {
      case (tree, None)         => tree
      case (f, Some(arguments)) => App(f, arguments)
    }

    P(app ~/ ("." ~/ variable ~/ ("(" ~/ nonEmptyArgs ~/ ")").?).rep).map {
      case (tree, selections) => dotSelection(tree, selections.toList)
    }
  }

  private def dotSelection(receiver: Tree, selections: List[(Tree, Option[NonEmptyList[Tree]])]): Tree =
    selections match {
      case Nil => receiver
      case (firstMethod, maybeArgs) :: nextSelections =>
        dotSelection(
          App(firstMethod, NonEmptyList(receiver, maybeArgs.fold(List.empty[Tree])(_.toList))),
          nextSelections
        )
    }

  private def atomicOperand[_: P]: P[Tree] =
    specialSyntax | factor

  private def list[_: P]: P[Tree] = P("[" ~/ args ~/ "]").map(tree.SpecialSyntax.cons)

  private def doubleLit[_: P]: P[Tree] =
    numbers.doubleLiteral.map(d => if (d % 1 == 0) IntLiteral(d.toInt) else DoubleLiteral(d))

  private def boolean[_: P]: P[Tree] =
    (P("true").map(_ => true) | P("false").map(_ => false)).map(Bool)

  private def variable[_: P]: P[Tree] =
    P(identifier).map(Id(_)) ~/ Pass

  private def unaryOps[_: P]: P[Tree] =
    P("-").map(_ => Id("inverse")) |
      P("!").map(_ => Id("not"))

  private def unaryPrefixOp[_: P]: P[Tree] =
    P(unaryOps ~/ atomicOperand).map { case (op, e) => App.of(op, e) }

  private def args[_: P]: P[List[Tree]] = P(nonEmptyArgs.map(_.toList) | Pass.map(_ => Nil))

  private def nonEmptyArgs[_: P]: P[NonEmptyList[Tree]] =
    nonEmptyCsv(expression)

  private def nonEmptyCsv[T, _: P](p: => P[T]): P[NonEmptyList[T]] =
    P(p ~/ ("," ~/ nonEmptyCsv(p)).?).map { case (head, tail) => NonEmptyList(head, tail.map(_.toList).getOrElse(Nil)) }

  private def identifier[_: P]: P[String] =
    ((alpha | CharIn("@")) ~~ alphaNum.repX(1).?).!.map(_.toLowerCase)

  private def alpha[_: P]: P[Unit] = P(CharIn("a-zA-Z_"))
  private def alphaNum[_: P]: P[Unit] = P(CharIn("0-9_") | alpha)

  private def specialSyntax[_: P]: P[Tree] = special.zip | special.product | special.uniformChoice

  private object special {

    def zip[_: P]: P[Tree] =
      P(StringInIgnoreCase("zip") ~ "(" ~/ nonEmptyCsv(comprehensionBinding) ~/ ")" ~/ "in" ~/ expression).map {
        case (bindings, body) => tree.SpecialSyntax.zip(bindings.toList, body)
      }

    def product[_: P]: P[Tree] =
      P(StringInIgnoreCase("product") ~ "(" ~/ nonEmptyCsv(comprehensionBinding) ~/ ")" ~/ "in" ~/ expression).map {
        case (bindings, body) => tree.SpecialSyntax.product(bindings.toList, body)
      }

    def uniformChoice[_: P]: P[Tree] =
      P(IgnoreCase("uniformchoice") ~ "(" ~/ nonEmptyArgs ~/ ")")
        .map(_.toList)
        .map(tree.SpecialSyntax.uniformChoice)

    private def comprehensionBinding[_: P]: P[(String, Tree)] =
      P(identifier ~/ "<-" ~/ expression)
  }

  private object numbers {

    def digit[_: P]: P[Unit] =
      P(CharIn("0-9"))

    def floatDigits[_: P]: P[Unit] =
      P(digit.rep ~~ "." ~~ digit.repX(1))

    def intLiteral[_: P]: P[Int] =
      P("-".? ~~ digit.repX(1)).!.map(_.toInt)

    def doubleLiteral[_: P]: P[Double] =
      P("-".? ~~ (floatDigits | digit.repX(1)) ~~ exp.?).!.map(_.toDouble)

    def exp[_: P]: P[Unit] = P(CharIn("Ee") ~~ CharIn("+\\-").? ~~ digit.repX(1))
  }
}
