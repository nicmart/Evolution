package evolution.primitive
import fastparse.all
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi._

trait ParsersModule[F[_]] { self: WithAst[F] =>
  import ast._

  object Parsers {
    lazy val parser: Parser[Expr] =
      expr0 ~ End

    lazy val expr0: Parser[Expr] =
      P(lambda | expr1)

    lazy val expr1: Parser[Expr] =
      P(infix(expr2, ops0, expr1) | expr2)

    lazy val expr2: Parser[Expr] =
      P(unaryPrefixOp | infix(expr3, ops1, expr2) | expr3)

    lazy val expr3: Parser[Expr] =
      P(("(" ~ expr0 ~ ")") | number | variable | let | funcCall)

    def infix(a: Parser[Expr], op: Parser[PredefinedFunction], b: Parser[Expr]): Parser[Expr] =
      P(a ~ op ~ b).map { case (a, op, b) => Expr.FuncCall(op, List(a, b)) }

    // Operators in order of precedence
    lazy val ops0: Parser[PredefinedFunction] = P("+").map(_ => PredefinedFunction.Add)
    lazy val ops1: Parser[PredefinedFunction] = P("*").map(_ => PredefinedFunction.Multiply)

    lazy val number: Parser[Expr.Number] =
      numbers.doubleLiteral.map(Expr.Number(_))

    lazy val variable: Parser[Expr.Var] =
      P("$" ~~ identifier).map(Expr.Var(_))

    lazy val funcCall: Parser[Expr] =
      func0Call | P(functionName ~ "(" ~ args ~ ")").map { case (func, args) => Expr.FuncCall(func, args) }

    // Functions with 0 arity
    lazy val func0Call: Parser[Expr] =
      P("empty").map(_ => Expr.FuncCall(PredefinedFunction.Empty, Nil))

    lazy val unaryPrefixOp: Parser[Expr] =
      P("-" ~ expr3).map(e => Expr.FuncCall(PredefinedFunction.Inverse, List(e)))

    lazy val lambda: Parser[Expr] =
      P(identifier ~ "->" ~ expr0).map { case (identifier, body) => Expr.Lambda(Expr.Var(identifier), body) }

    lazy val let: Parser[Expr] = P("let(" ~ identifier ~ "," ~ expr0 ~ "," ~ expr0 ~ ")").map {
      case (id, value, in) =>
        Expr.Let(Expr.Var(id), value, in)
    }

    lazy val args: Parser[List[Expr]] =
      P(expr0 ~ ("," ~ args).?).map { case (head, tail) => head :: tail.getOrElse(Nil) }

    lazy val identifier: Parser[String] = (alpha ~~ alphaNum.repX(1).?).!
    lazy val functionName: Parser[PredefinedFunction] = identifier.map(PredefinedFunction.withNameInsensitive)
    lazy val alpha: Parser[Unit] = P(CharIn('a' to 'z') | CharIn('A' to 'Z'))
    lazy val alphaNum: Parser[Unit] = P(CharIn('0' to '9') | alpha)

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
}
