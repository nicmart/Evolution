package evolution.language
import contextual._
import evolution.language.ParserConfig.White._
import evolution.language.ParserConfig.whitespaces
import fastparse.noApi._

trait ParserModule[F[_]] { self: ASTModule[F] with PredefinedConstantsModule[F] =>

  object Parsers {
    def parse(astString: String): Either[String, AST] =
      Parsers.parser
        .parse(astString)
        .fold((_, failIndex, extra) => Left(s"Failed at $failIndex: ${extra.traced.trace}"), (expr, _) => Right(expr))

    val parser: Parser[AST] =
      P(whitespaces ~ ast ~ End)

    def binaryOperators: List[(String, AST)] = precedenceGroups.allOperators

    private lazy val ast: Parser[AST] =
      P(lambdaOrLet | precedenceGroups.parser)

    // Operator groups, order by ascending Precedence
    private lazy val precedenceGroups: PrecedenceGroups = PrecedenceGroups(
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
          "@+" -> AST.App(AST.Identifier(Constant1.Lift.entryName), AST.Identifier(Constant2.Add.entryName)),
          "-" -> AST.Identifier(Constant2.Minus.entryName)
        ),
        PrecedenceGroup(
          "*" -> AST.Identifier(Constant2.Multiply.entryName),
          "/" -> AST.Identifier(Constant2.Div.entryName),
          "%" -> AST.Identifier(Constant2.Mod.entryName),
          "@*" -> AST.App(AST.Identifier(Constant1.Lift.entryName), AST.Identifier(Constant2.Multiply.entryName))
        ),
        PrecedenceGroup(
          "^" -> AST.Identifier(Constant2.Exp.entryName)
        )
      )
    )

    private lazy val factor: Parser[AST] =
      P(("(" ~ ast ~ ")") | number | boolean | unaryPrefixOp | let | variable | lifted | list)

    private lazy val appOrFactor: Parser[AST] =
      P(factor ~ ("(" ~/ nonEmptyArgs ~ ")").?).map {
        case (f, None)       => f
        case (f, Some(args)) => evalApp(f, args)
      }

    private lazy val list: Parser[AST] = P("[" ~/ args ~ "]").map(evalList)

    private lazy val number: Parser[AST.Number] =
      numbers.doubleLiteral.map(AST.Number(_))

    private lazy val boolean: Parser[AST.Bool] =
      (P("true").map(_ => true) | P("false").map(_ => false)).map(AST.Bool(_))

    private lazy val variable: Parser[AST.Identifier] =
      P(identifier).map(AST.Identifier(_))

    private lazy val unaryOps: Parser[AST.Identifier] =
      P("-").map(_ => AST.Identifier(Constant1.Inverse.entryName)) |
        P("!").map(_ => AST.Identifier(Constant1.Not.entryName))

    private lazy val unaryPrefixOp: Parser[AST] =
      P(unaryOps ~ appOrFactor).map { case (op, e) => AST.App(op, e) }

    private lazy val lambdaOrLet: Parser[AST] = {
      def lambdaTail(id: String): Parser[AST] = P(whitespaces ~ "->" ~/ ast).map { expr =>
        AST.Lambda(id, expr)
      }
      def letTail(id: String): Parser[AST] =
        P(whitespaces ~ "=" ~/ ast ~ "in" ~/ ast).map {
          case (value, body) =>
            AST.Let(id, value, body)
        }
      def tail(id: String): Parser[AST] = lambdaTail(id) | letTail(id)

      identifier.flatMap(tail)
    }

    private lazy val let: Parser[AST] = P("let(" ~/ identifier ~ "," ~ ast ~ "," ~ ast ~ ")").map {
      case (id, value, in) =>
        AST.Let(id, value, in)
    }

    private lazy val args: Parser[List[AST]] = P(nonEmptyArgs | PassWith(Nil))

    private lazy val nonEmptyArgs: Parser[List[AST]] =
      P(ast ~ ("," ~ nonEmptyArgs).?).map { case (head, tail) => head :: tail.getOrElse(Nil) }

    private lazy val identifier: Parser[String] = (alpha ~~ alphaNum.repX(1).?).!.map(_.toLowerCase)

    private lazy val lifted: Parser[AST] =
      P("@" ~/ factor).map(ast => AST.App(AST.Identifier(Constant1.Lift.entryName), ast))

    private lazy val alpha: Parser[Unit] = P(CharIn('a' to 'z') | CharIn('A' to 'Z'))
    private lazy val alphaNum: Parser[Unit] = P(CharIn('0' to '9') | alpha)

    private def evalApp(f: AST, args: List[AST]): AST =
      args match {
        case Nil                => f
        case argHead :: argTail => evalApp(AST.App(f, argHead), argTail)
      }

    private def evalList(asts: List[AST]): AST = asts match {
      case Nil          => AST.Identifier(Constant0.Empty.entryName)
      case head :: tail => AST.App2(AST.Identifier(Constant2.Cons.entryName), head, evalList(tail))
    }

    private object numbers {

      lazy val digit: Parser[Unit] =
        P(CharIn('0' to '9'))

      lazy val floatDigits: Parser[Unit] =
        P(digit.rep ~~ "." ~~ digit.repX(1))

      lazy val intLiteral: Parser[Int] =
        digit.!.map(_.toInt)

      lazy val doubleLiteral: Parser[String] =
        P("-".? ~~ (floatDigits | digit.repX(1)) ~~ exp.?).!

      lazy val exp: Parser[Unit] = P(CharIn("Ee") ~~ CharIn("+\\-").? ~~ digit.repX(1))
    }
  }

  private[language] case class PrecedenceGroup(operators: (String, AST)*) {
    def parser(next: Parser[AST]): Parser[AST] = P(next ~ (opsParser ~/ next).rep).map {
      case (head, tail) => evalAssocBinaryOp(head, tail.toList)
    }

    private def opsParser: Parser[AST] = operators.foldLeft[Parser[AST]](Fail) {
      case (accParser, (opString, ast)) =>
        accParser | P(opString).map(_ => ast)
    }

    private def evalAssocBinaryOp(head: AST, tail: List[(AST, AST)]): AST =
      tail match {
        case Nil                        => head
        case (op, tailHead) :: tailTail => AST.App(AST.App(op, head), evalAssocBinaryOp(tailHead, tailTail))
      }
  }

  private[language] case class PrecedenceGroups(last: Parser[AST], groups: List[PrecedenceGroup]) {
    def parser: Parser[AST] = groups.foldRight(last) { (group, accParser) =>
      group.parser(accParser)
    }

    def allOperators: List[(String, AST)] = groups.flatMap(group => group.operators)
  }

  object ASTInterpolator extends Interpolator {
    type Out = AST

    def contextualize(interpolation: StaticInterpolation) = {
      val lit @ Literal(_, astString) = interpolation.parts.head
      if (Parsers.parse(astString).isLeft)
        interpolation.abort(lit, 0, "not a valid URL")

      Nil
    }

    def evaluate(interpolation: RuntimeInterpolation): AST =
      Parsers.parse(interpolation.literals.head).right.get
  }

  implicit class ASTStringContext(sc: StringContext) {
    val ast = Prefix(ASTInterpolator, sc)
  }
}
