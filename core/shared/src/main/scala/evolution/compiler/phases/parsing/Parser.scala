package evolution.compiler.phases.parsing

import evolution.compiler.phases.parsing
import evolution.compiler.phases.parsing.ParserConfig._
import fastparse._
import evolution.compiler.phases.parsing.ParserConfig.whitespaces
import evolution.compiler.phases.typing.config.{ Constant1, Constant2 }
import evolution.compiler.ast.TreeF._
import evolution.compiler.ast.TreeF
import evolution.compiler.ast
import cats.data.NonEmptyList

object Parser {
  def parse(astString: String): Either[ParserFailure, Tree] =
    fastparse
      .parse(astString, program(_))
      .fold((_, failIndex, extra) => Left(new ParserFailure(failIndex, extra)), (expr, _) => Right(expr))

  def binaryOperators: List[(String, Tree)] = allPrecedenceGroups.flatMap(group => group.operators)

  private def program[_: P]: P[Tree] =
    P(whitespaces ~ expression ~ End)

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
      TreeF.Lambda(_, expr).embed
    }

    // `a = 2 in` ... and `a == 2` share the same prefix, so the cut must be after the negative lookahead
    private def letTail[_: P]: P[String => Tree] =
      P(whitespaces ~ "=" ~ !"=" ~/ expression ~/ "in" ~/ expression).map {
        case (value, body) =>
          TreeF.Let(_, value, body).embed
      }

    private def sampleTail[_: P]: P[String => Tree] =
      P(whitespaces ~ "<-" ~/ expression ~/ "in" ~/ expression).map {
        case (sampling, body) =>
          variable => ast.SpecialSyntax.withFirst(variable -> sampling, body)
      }

    // We need to allow backtracking, since f(x, y) can be a function application in addition to a binding
    private def argsTail[_: P]: P[String => Tree] =
      P(whitespaces ~~ "(" ~ NoCut(nonEmptyCsv(identifier)) ~ ")" ~ "=" ~ !"=" ~/ expression ~/ "in" ~/ expression).map {
        case (args, value, body) => name => ast.SpecialSyntax.functionBinding(name, args.toList, value, body)
      }
  }

  private val allPrecedenceGroups = List(
    PrecedenceGroup(
      "||" -> Identifier(Constant2.Or.entryName).embed
    ),
    PrecedenceGroup(
      "&&" -> Identifier(Constant2.And.entryName).embed
    ),
    PrecedenceGroup(
      ">=" -> Identifier(Constant2.GreaterThanOrEqual.entryName).embed,
      ">" -> Identifier(Constant2.GreaterThan.entryName).embed,
      "<=" -> Identifier(Constant2.LessThanOrEqual.entryName).embed,
      "<" -> Identifier(Constant2.LessThan.entryName).embed
    ),
    PrecedenceGroup(
      "==" -> Identifier(Constant2.Eq.entryName).embed,
      "!=" -> Identifier(Constant2.Neq.entryName).embed
    ),
    PrecedenceGroup(
      "+" -> Identifier(Constant2.Add.entryName).embed,
      "-" -> Identifier(Constant2.Minus.entryName).embed
    ),
    PrecedenceGroup(
      "*" -> Identifier(Constant2.Multiply.entryName).embed,
      "/" -> Identifier(Constant2.Div.entryName).embed,
      "%" -> Identifier(Constant2.Mod.entryName).embed
    ),
    PrecedenceGroup(
      "^" -> Identifier(Constant2.Exp.entryName).embed
    )
  )

  // Operator groups, order by ascending Precedence
  private def precedenceGroups[_: P]: PrecedenceGroups = parsing.PrecedenceGroups(
    () => atomicOperand,
    allPrecedenceGroups
  )

  private def factor[_: P]: P[Tree] =
    P(
      ("(" ~/ expression ~/ ")") | doubleLit | boolean | unaryPrefixOp |
        variable | list
    )

  private def atomicOperand[_: P]: P[Tree] =
    specialSyntax | P(factor ~/ ("(" ~/ nonEmptyArgs ~/ ")").?).map {
      case (f, None)            => f
      case (f, Some(arguments)) => App(f, arguments).embed
    }

  private def list[_: P]: P[Tree] = P("[" ~/ args ~/ "]").map(ConsN)

  private def doubleLit[_: P]: P[Tree] =
    numbers.doubleLiteral.map(d => if (d % 1 == 0) IntLiteral(d.toInt).embed else DoubleLiteral(d).embed)

  private def boolean[_: P]: P[Tree] =
    (P("true").map(_ => true) | P("false").map(_ => false)).map(Bool).map(_.embed)

  private def variable[_: P]: P[Tree] =
    P(identifier).map(Identifier(_).embed) ~/ Pass

  private def unaryOps[_: P]: P[Tree] =
    P("-").map(_ => Identifier(Constant1.Inverse.entryName).embed) |
      P("!").map(_ => Identifier(Constant1.Not.entryName).embed)

  private def unaryPrefixOp[_: P]: P[Tree] =
    P(unaryOps ~/ atomicOperand).map { case (op, e) => AppN(op, e) }

  private def args[_: P]: P[List[Tree]] = P(nonEmptyArgs.map(_.toList) | Pass.map(_ => Nil))

  private def nonEmptyArgs[_: P]: P[NonEmptyList[Tree]] =
    nonEmptyCsv(expression)

  private def nonEmptyCsv[T, _: P](p: => P[T]): P[NonEmptyList[T]] =
    P(p ~/ ("," ~/ nonEmptyCsv(p)).?).map { case (head, tail) => NonEmptyList(head, tail.map(_.toList).getOrElse(Nil)) }

  private def identifier[_: P]: P[String] =
    ((alpha | CharIn("@")) ~~ alphaNum.repX(1).?).!.map(_.toLowerCase)

  private def alpha[_: P]: P[Unit] = P(CharIn("a-z") | CharIn("A-Z"))
  private def alphaNum[_: P]: P[Unit] = P(CharIn("0-9") | alpha)

  private def specialSyntax[_: P]: P[Tree] = SpecialSyntax.zip | SpecialSyntax.product | SpecialSyntax.uniformChoice

  private object SpecialSyntax {
    def zip[_: P]: P[Tree] =
      P(StringInIgnoreCase("zip") ~ "(" ~/ nonEmptyCsv(comprehensionBinding) ~/ ")" ~/ "in" ~/ expression).map {
        case (bindings, body) => ast.SpecialSyntax.zip(bindings.toList, body)
      }

    def product[_: P]: P[Tree] =
      P(StringInIgnoreCase("product") ~ "(" ~/ nonEmptyCsv(comprehensionBinding) ~/ ")" ~/ "in" ~/ expression).map {
        case (bindings, body) => ast.SpecialSyntax.product(bindings.toList, body)
      }

    def uniformChoice[_: P]: P[Tree] =
      P(IgnoreCase(Constant1.UniformChoice.entryName) ~ "(" ~/ nonEmptyArgs ~/ ")")
        .map(_.toList)
        .map(ast.SpecialSyntax.uniformChoice)

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
