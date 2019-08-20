package evolution.compiler.phases.parsing

import evolution.compiler.ast.AST
import evolution.compiler.phases.parsing
import evolution.compiler.phases.parsing.ParserConfig.White._
import evolution.compiler.phases.parsing.ParserConfig.whitespaces
import evolution.compiler.phases.typing.config.{ Constant1, Constant2 }
import fastparse.noApi._

object Parser {
  def parse(astString: String): Either[ParserFailure, AST] =
    Parser.program
      .parse(astString)
      .fold((_, failIndex, extra) => Left(new ParserFailure(failIndex, extra)), (expr, _) => Right(expr))

  def binaryOperators: List[(String, AST)] = precedenceGroups.allOperators

  private val program: Parser[AST] =
    P(whitespaces ~ expression ~ End)

  private lazy val expression: Parser[AST] =
    P(lambdaOrLet | precedenceGroups.parser)

  private lazy val lambdaOrLet: Parser[AST] = {
    def lambdaTail: Parser[String => AST] = P(whitespaces ~ "->" ~/ expression).map { expr =>
      AST.Lambda(_, expr)
    }

    // `a = 2 in` ... and `a == 2` share the same prefix, so the cut must be after the negative lookahead
    def letTail: Parser[String => AST] =
      P(whitespaces ~ "=" ~ !"=" ~/ expression ~/ "in" ~/ expression).map {
        case (value, body) =>
          AST.Let(_, value, body)
      }

    def sampleTail: Parser[String => AST] =
      P(whitespaces ~ "<-" ~/ expression ~/ "in" ~/ expression).map {
        case (sampling, body) =>
          variable => AST.SpecialSyntax.withFirst(variable -> sampling, body)
      }

    // Note: a previous solution based on flatMap caused undesired back-tracking
    P(identifier ~ (lambdaTail | letTail | sampleTail)).map {
      case (id, f) => f(id)
    }
  }

  // Operator groups, order by ascending Precedence
  private lazy val precedenceGroups: PrecedenceGroups = parsing.PrecedenceGroups(
    appOrFactor,
    List(
      PrecedenceGroup(
        "||" -> AST.Identifier(Constant2.Or.entryName)
      ),
      PrecedenceGroup(
        "&&" -> AST.Identifier(Constant2.And.entryName)
      ),
      PrecedenceGroup(
        ">=" -> AST.Identifier(Constant2.GreaterThanOrEqual.entryName),
        ">" -> AST.Identifier(Constant2.GreaterThan.entryName),
        "<=" -> AST.Identifier(Constant2.LessThanOrEqual.entryName),
        "<" -> AST.Identifier(Constant2.LessThan.entryName)
      ),
      PrecedenceGroup(
        "==" -> AST.Identifier(Constant2.Eq.entryName),
        "!=" -> AST.Identifier(Constant2.Neq.entryName)
      ),
      PrecedenceGroup(
        "+" -> AST.Identifier(Constant2.Add.entryName),
        "-" -> AST.Identifier(Constant2.Minus.entryName)
      ),
      PrecedenceGroup(
        "*" -> AST.Identifier(Constant2.Multiply.entryName),
        "/" -> AST.Identifier(Constant2.Div.entryName),
        "%" -> AST.Identifier(Constant2.Mod.entryName)
      ),
      PrecedenceGroup(
        "^" -> AST.Identifier(Constant2.Exp.entryName)
      )
    )
  )

  private lazy val factor: Parser[AST] =
    P(
      ("(" ~/ expression ~/ ")") | doubleLit | boolean | unaryPrefixOp |
        variable | list
    )

  private lazy val appOrFactor: Parser[AST] =
    specialSyntax | P(factor ~/ ("(" ~/ nonEmptyArgs ~/ ")").?).map {
      case (f, None)            => f
      case (f, Some(arguments)) => AST.AppN(f, arguments: _*)
    }

  private lazy val list: Parser[AST] = P("[" ~/ args ~/ "]").map(AST.ConsN)

  private lazy val doubleLit: Parser[AST] =
    numbers.doubleLiteral.map(d => if (d % 1 == 0) AST.IntLiteral(d.toInt) else AST.DoubleLiteral(d))

  private lazy val boolean: Parser[AST.Bool] =
    (P("true").map(_ => true) | P("false").map(_ => false)).map(AST.Bool(_))

  private lazy val variable: Parser[AST.Identifier] =
    P(identifier).map(AST.Identifier(_)) ~/ Pass

  private lazy val unaryOps: Parser[AST.Identifier] =
    P("-").map(_ => AST.Identifier(Constant1.Inverse.entryName)) |
      P("!").map(_ => AST.Identifier(Constant1.Not.entryName))

  private lazy val unaryPrefixOp: Parser[AST] =
    P(unaryOps ~/ appOrFactor).map { case (op, e) => AST.App(op, e) }

  private lazy val args: Parser[List[AST]] = P(nonEmptyArgs | PassWith(Nil))

  private lazy val nonEmptyArgs: Parser[List[AST]] =
    nonEmptyCsv(expression)

  private def nonEmptyCsv[T](p: Parser[T]): Parser[List[T]] =
    P(p ~/ ("," ~/ nonEmptyCsv(p)).?).map { case (head, tail) => head :: tail.getOrElse(Nil) }

  private lazy val identifier: Parser[String] =
    ((alpha | CharIn(Seq('@'))) ~~ alphaNum.repX(1).?).!.map(_.toLowerCase)

  private lazy val alpha: Parser[Unit] = P(CharIn('a' to 'z') | CharIn('A' to 'Z'))
  private lazy val alphaNum: Parser[Unit] = P(CharIn('0' to '9') | alpha)

  private lazy val specialSyntax: Parser[AST] = zip

  private lazy val zip: Parser[AST] =
    P("zip" ~/ "(" ~/ nonEmptyCsv(zipBinding) ~/ ")" ~/ "in" ~/ expression).map {
      case (bindings, body) => AST.SpecialSyntax.zip(bindings, body)
    }

  private lazy val zipBinding: Parser[(String, AST)] =
    P(identifier ~/ "<-" ~/ expression)

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
