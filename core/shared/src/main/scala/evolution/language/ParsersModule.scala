package evolution.language
import contextual._
import fastparse.all
import ParserConfig.White._
import ParserConfig.whitespaces
import fastparse.noApi._

trait ParsersModule[F[_]] { self: ASTModule[F] =>

  def parse(astString: String): Either[String, AST] =
    Parsers.parser
      .parse(astString)
      .fold((_, failIndex, extra) => Left(s"Failed at $failIndex: ${extra.traced.trace}"), (expr, _) => Right(expr))

  object Parsers {
    lazy val parser: Parser[AST] =
      P(whitespaces ~ precedence0 ~ End)

    lazy val precedence0: Parser[AST] =
      P(lambdaOrLet | precedence1)

    lazy val precedence1Ops: Parser[AST] =
      P("||").map(_ => AST.Const(Constant.Or))

    lazy val precedence1: Parser[AST] =
      P(precedence2 ~ (precedence1Ops ~/ precedence2).rep).map {
        case (head, tail) => evalAssocBinaryOp(head, tail.toList)
      }

    lazy val precedence2Ops: Parser[AST] =
      P("&&").map(_ => AST.Const(Constant.And))

    lazy val precedence2: Parser[AST] =
      P(precedence3 ~ (precedence2Ops ~/ precedence3).rep).map {
        case (head, tail) => evalAssocBinaryOp(head, tail.toList)
      }

    lazy val precedence3Ops: Parser[AST] =
      P("+").map(_ => AST.Const(Constant.Add)) |
        P("<+>").map(_ => AST.App(AST.Const(Constant.Lift), AST.Const(Constant.Add)))

    lazy val precedence3: Parser[AST] =
      P(precedence4 ~ (precedence3Ops ~/ precedence4).rep).map {
        case (head, tail) => evalAssocBinaryOp(head, tail.toList)
      }

    lazy val precedence4Ops: Parser[AST] =
      P("*").map(_ => AST.Const(Constant.Multiply)) |
        P("/").map(_ => AST.Const(Constant.Div)) |
        P("%").map(_ => AST.Const(Constant.Mod)) |
        P("<*>").map(_ => AST.App(AST.Const(Constant.Lift), AST.Const(Constant.Multiply)))

    lazy val precedence4: Parser[AST] =
      P(precedence5 ~ (precedence4Ops ~/ precedence5).rep).map {
        case (head, tail) => evalAssocBinaryOp(head, tail.toList)
      }

    lazy val precedence5Ops: Parser[AST] =
      P("^").map(_ => AST.Const(Constant.Exp))

    lazy val precedence5: Parser[AST] =
      P(appOrFactor ~ (precedence5Ops ~/ appOrFactor).rep).map {
        case (head, tail) => evalAssocBinaryOp(head, tail.toList)
      }

    lazy val factor: Parser[AST] =
      P(("(" ~ precedence0 ~ ")") | number | boolean | unaryPrefixOp | variable | let | lifted | predefinedConstant | list)

    lazy val appOrFactor: Parser[AST] =
      P(factor ~ ("(" ~/ nonEmptyArgs ~ ")").?).map {
        case (f, None)       => f
        case (f, Some(args)) => evalApp(f, args)
      }

    lazy val list: Parser[AST] = P("[" ~/ args ~ "]").map(evalList)

    lazy val number: Parser[AST.Number] =
      numbers.doubleLiteral.map(AST.Number(_))

    lazy val boolean: Parser[AST.Bool] =
      (P("true").map(_ => true) | P("false").map(_ => false)).map(AST.Bool(_))

    lazy val variable: Parser[AST.Var] =
      P("$" ~~ identifier).map(AST.Var(_))

    lazy val unaryOps: Parser[AST.Const] =
      P("-").map(_ => AST.Const(Constant.Inverse)) |
        P("!").map(_ => AST.Const(Constant.Not))

    lazy val unaryPrefixOp: Parser[AST] =
      P(unaryOps ~ appOrFactor).map { case (op, e) => AST.App(op, e) }

    lazy val lambdaOrLet: Parser[AST] = {
      def lambdaTail(id: String): Parser[AST] = P(whitespaces ~ "->" ~/ precedence0).map { expr =>
        AST.Lambda(AST.Var(id), expr)
      }
      def letTail(id: String): Parser[AST] =
        P(whitespaces ~ "=" ~/ precedence0 ~ "in" ~/ precedence0).map {
          case (value, body) =>
            AST.Let(AST.Var(id), value, body)
        }
      def tail(id: String): Parser[AST] = lambdaTail(id) | letTail(id)

      identifier.flatMap(tail)
    }

    lazy val let: Parser[AST] = P("let(" ~/ identifier ~ "," ~ precedence0 ~ "," ~ precedence0 ~ ")").map {
      case (id, value, in) =>
        AST.Let(AST.Var(id), value, in)
    }

    lazy val args: Parser[List[AST]] = P(nonEmptyArgs | PassWith(Nil))

    lazy val nonEmptyArgs: Parser[List[AST]] =
      P(precedence0 ~ ("," ~ nonEmptyArgs).?).map { case (head, tail) => head :: tail.getOrElse(Nil) }

    lazy val identifier: Parser[String] = (alpha ~~ alphaNum.repX(1).?).!

    lazy val lifted: Parser[AST] =
      P("<" ~/ precedence0 ~ ">").map(ast => AST.App(AST.Const(Constant.Lift), ast))

    lazy val predefinedConstant: Parser[AST.Const] = identifier
      .filter(id => Constant.lowerCaseNamesToValuesMap.isDefinedAt(id.toLowerCase))
      .map(Constant.withNameInsensitive)
      .map(AST.Const(_))

    lazy val alpha: Parser[Unit] = P(CharIn('a' to 'z') | CharIn('A' to 'Z'))
    lazy val alphaNum: Parser[Unit] = P(CharIn('0' to '9') | alpha)

    def evalAssocBinaryOp(head: AST, tail: List[(AST, AST)]): AST =
      tail match {
        case Nil                        => head
        case (op, tailHead) :: tailTail => AST.App(AST.App(op, head), evalAssocBinaryOp(tailHead, tailTail))
      }

    def evalApp(f: AST, args: List[AST]): AST =
      args match {
        case Nil                => f
        case argHead :: argTail => evalApp(AST.App(f, argHead), argTail)
      }

    def evalList(asts: List[AST]): AST = asts match {
      case Nil          => AST.Const(Constant.Empty)
      case head :: tail => AST.App2(AST.Const(Constant.Cons), head, evalList(tail))
    }

    object numbers {

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

  object ASTInterpolator extends Interpolator {
    type Out = AST

    def contextualize(interpolation: StaticInterpolation) = {
      val lit @ Literal(_, astString) = interpolation.parts.head
      if (parse(astString).isLeft)
        interpolation.abort(lit, 0, "not a valid URL")

      Nil
    }

    def evaluate(interpolation: RuntimeInterpolation): AST =
      parse(interpolation.literals.head).right.get
  }

  implicit class ASTStringContext(sc: StringContext) {
    val ast = Prefix(ASTInterpolator, sc)
  }
}
