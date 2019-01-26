package evolution.primitive
import fastparse.all
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi._

trait ParsersModule[F[_]] { self: WithAst[F] =>
  import ast._

  object Parsers {
    lazy val parser: Parser[Expr] =
      P(precedence0 ~ End)

    lazy val precedence0: Parser[Expr] =
      P(lambda | precedence1)

    lazy val precedence1: Parser[Expr] =
      P(precedence2 ~ ("+" ~/ precedence2).rep).map {
        case (head, tail) => evalAssocBinaryOp(head, tail.toList, PredefinedFunction.Add)
      }

    lazy val precedence2: Parser[Expr] =
      P(factor ~ ("*" ~/ factor).rep).map {
        case (head, tail) => evalAssocBinaryOp(head, tail.toList, PredefinedFunction.Multiply)
      }

    lazy val factor: Parser[Expr] =
      P(("(" ~ precedence0 ~ ")") | number | unaryPrefixOp | variable | let | funcCall)

    lazy val number: Parser[Expr.Number] =
      numbers.doubleLiteral.map(Expr.Number(_))

    lazy val variable: Parser[Expr.Var] =
      P("$" ~~ identifier).map(Expr.Var(_))

    lazy val funcCall: Parser[Expr] =
      func0Call | P(functionName ~ "(" ~/ args ~ ")").map { case (func, args) => Expr.FuncCall(func, args) }

    // Functions with 0 arity
    lazy val func0Call: Parser[Expr] =
      P("empty").map(_ => Expr.FuncCall(PredefinedFunction.Empty, Nil))

    lazy val unaryPrefixOp: Parser[Expr] =
      P("-" ~ factor).map(e => Expr.FuncCall(PredefinedFunction.Inverse, List(e)))

    lazy val lambda: Parser[Expr] =
      P(identifier ~ "->" ~/ precedence0).map { case (identifier, body) => Expr.Lambda(Expr.Var(identifier), body) }

    lazy val let: Parser[Expr] = P("let(" ~/ identifier ~ "," ~ precedence0 ~ "," ~ precedence0 ~ ")").map {
      case (id, value, in) =>
        Expr.Let(Expr.Var(id), value, in)
    }

    lazy val args: Parser[List[Expr]] =
      P(precedence0 ~ ("," ~ args).?).map { case (head, tail) => head :: tail.getOrElse(Nil) }

    lazy val identifier: Parser[String] = (alpha ~~ alphaNum.repX(1).?).!

    lazy val functionName: Parser[PredefinedFunction] = identifier
      .filter(id => PredefinedFunction.lowerCaseNamesToValuesMap.isDefinedAt(id.toLowerCase))
      .map(PredefinedFunction.withNameInsensitive)
    lazy val alpha: Parser[Unit] = P(CharIn('a' to 'z') | CharIn('A' to 'Z'))
    lazy val alphaNum: Parser[Unit] = P(CharIn('0' to '9') | alpha)

    def evalAssocBinaryOp(head: Expr, tail: List[Expr], op: PredefinedFunction): Expr =
      (head :: tail).reduceRight[Expr] { (e, m) =>
        Expr.FuncCall(op, List(e, m))
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
}
