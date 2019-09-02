package evolution.compilertree.phases.parsing

import evolution.compilertree.phases.parsing
import evolution.compilertree.phases.parsing.ParserConfig.White._
import evolution.compilertree.phases.parsing.ParserConfig.whitespaces
import evolution.compilertree.phases.typing.config.{ Constant1, Constant2 }
import fastparse.noApi._
import evolution.compilertree.ast.TreeF._
import evolution.compilertree.ast.TreeF
import evolution.compilertree.ast
import cats.data.NonEmptyList

object Parser {
  def parse(astString: String): Either[ParserFailure, Tree] =
    Parser.program
      .parse(astString)
      .fold((_, failIndex, extra) => Left(new ParserFailure(failIndex, extra)), (expr, _) => Right(expr))

  def binaryOperators: List[(String, Tree)] = precedenceGroups.allOperators

  private val program: Parser[Tree] =
    P(whitespaces ~ expression ~ End)

  private lazy val expression: Parser[Tree] =
    P(notOperand | precedenceGroups.operand)

  // expressions that can't be an operand of a binary operator
  private lazy val notOperand: Parser[Tree] = {
    def lambdaTail: Parser[String => Tree] = P(whitespaces ~ "->" ~/ expression).map { expr =>
      TreeF.Lambda(_, expr).embed
    }

    // `a = 2 in` ... and `a == 2` share the same prefix, so the cut must be after the negative lookahead
    def letTail: Parser[String => Tree] =
      P(whitespaces ~ "=" ~ !"=" ~/ expression ~/ "in" ~/ expression).map {
        case (value, body) =>
          TreeF.Let(_, value, body).embed
      }

    def sampleTail: Parser[String => Tree] =
      P(whitespaces ~ "<-" ~/ expression ~/ "in" ~/ expression).map {
        case (sampling, body) =>
          variable => ast.SpecialSyntax.withFirst(variable -> sampling, body)
      }

    // We need to allow backtracking, since f(x, y) can be a function application in addition to a binding
    def argsTail: Parser[String => Tree] =
      P(whitespaces ~~ "(" ~ NoCut(nonEmptyCsv(identifier)) ~ ")" ~ "=" ~ !"=" ~/ expression ~/ "in" ~/ expression).map {
        case (args, value, body) => name => ast.SpecialSyntax.functionBinding(name, args.toList, value, body)
      }

    // Note: a previous solution based on flatMap caused undesired back-tracking
    P(identifier ~ (lambdaTail | letTail | sampleTail | argsTail)).map {
      case (id, f) => f(id)
    }
  }

  // Operator groups, order by ascending Precedence
  private lazy val precedenceGroups: PrecedenceGroups = parsing.PrecedenceGroups(
    atomicOperand,
    List(
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
  )

  private lazy val factor: Parser[Tree] =
    P(
      ("(" ~/ expression ~/ ")") | doubleLit | boolean | unaryPrefixOp |
        variable | list
    )

  private lazy val atomicOperand: Parser[Tree] =
    specialSyntax | P(factor ~/ ("(" ~/ nonEmptyArgs ~/ ")").?).map {
      case (f, None)            => f
      case (f, Some(arguments)) => App(f, arguments).embed
    }

  private lazy val list: Parser[Tree] = P("[" ~/ args ~/ "]").map(ConsN)

  private lazy val doubleLit: Parser[Tree] =
    numbers.doubleLiteral.map(d => if (d % 1 == 0) IntLiteral(d.toInt).embed else DoubleLiteral(d).embed)

  private lazy val boolean: Parser[Tree] =
    (P("true").map(_ => true) | P("false").map(_ => false)).map(Bool).map(_.embed)

  private lazy val variable: Parser[Tree] =
    P(identifier).map(Identifier(_).embed) ~/ Pass

  private lazy val unaryOps: Parser[Tree] =
    P("-").map(_ => Identifier(Constant1.Inverse.entryName).embed) |
      P("!").map(_ => Identifier(Constant1.Not.entryName).embed)

  private lazy val unaryPrefixOp: Parser[Tree] =
    P(unaryOps ~/ atomicOperand).map { case (op, e) => AppN(op, e) }

  private lazy val args: Parser[List[Tree]] = P(nonEmptyArgs.map(_.toList) | PassWith(Nil))

  private lazy val nonEmptyArgs: Parser[NonEmptyList[Tree]] =
    nonEmptyCsv(expression)

  private def nonEmptyCsv[T](p: Parser[T]): Parser[NonEmptyList[T]] =
    P(p ~/ ("," ~/ nonEmptyCsv(p)).?).map { case (head, tail) => NonEmptyList(head, tail.map(_.toList).getOrElse(Nil)) }

  private lazy val identifier: Parser[String] =
    ((alpha | CharIn(Seq('@'))) ~~ alphaNum.repX(1).?).!.map(_.toLowerCase)

  private lazy val alpha: Parser[Unit] = P(CharIn('a' to 'z') | CharIn('A' to 'Z'))
  private lazy val alphaNum: Parser[Unit] = P(CharIn('0' to '9') | alpha)

  private lazy val specialSyntax: Parser[Tree] = SpecialSyntax.zip | SpecialSyntax.product | SpecialSyntax.uniformChoice

  private object SpecialSyntax {
    lazy val zip: Parser[Tree] =
      P(StringInIgnoreCase("zip") ~ "(" ~/ nonEmptyCsv(comprehensionBinding) ~/ ")" ~/ "in" ~/ expression).map {
        case (bindings, body) => ast.SpecialSyntax.zip(bindings.toList, body)
      }

    lazy val product: Parser[Tree] =
      P(StringInIgnoreCase("product") ~ "(" ~/ nonEmptyCsv(comprehensionBinding) ~/ ")" ~/ "in" ~/ expression).map {
        case (bindings, body) => ast.SpecialSyntax.product(bindings.toList, body)
      }

    lazy val uniformChoice: Parser[Tree] =
      P(StringInIgnoreCase(Constant1.UniformChoice.entryName) ~ "(" ~/ nonEmptyArgs ~/ ")")
        .map(_.toList)
        .map(ast.SpecialSyntax.uniformChoice)

    private lazy val comprehensionBinding: Parser[(String, Tree)] =
      P(identifier ~/ "<-" ~/ expression)
  }

  private object numbers {

    lazy val digit: Parser[Unit] =
      P(CharIn('0' to '9'))

    lazy val floatDigits: Parser[Unit] =
      P(digit.rep ~~ "." ~~ digit.repX(1))

    lazy val intLiteral: Parser[Int] =
      P("-".? ~~ digit.repX(1)).!.map(_.toInt)

    lazy val doubleLiteral: Parser[Double] =
      P("-".? ~~ (floatDigits | digit.repX(1)) ~~ exp.?).!.map(_.toDouble)

    lazy val exp: Parser[Unit] = P(CharIn("Ee") ~~ CharIn("+\\-").? ~~ digit.repX(1))
  }
}
