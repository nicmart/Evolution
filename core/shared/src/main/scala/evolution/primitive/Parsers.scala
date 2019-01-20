package evolution.primitive
import fastparse.all
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi._

class Parsers[F[_]](val ast: Ast[F]) {
  import ast._

  lazy val parser: Parser[Expr] =
    expr0 ~ End

  lazy val expr0: Parser[Expr] =
    P(lambda | expr1)

  lazy val expr1: Parser[Expr] =
    P(infix(expr2, ops0, expr1) | expr2)

  lazy val expr2: Parser[Expr] =
    P(infix(expr3, ops1, expr2) | expr3)

  lazy val expr3: Parser[Expr] =
    P(("(" ~ expr0 ~ ")") | number | variable | funcCall)

  def infix(a: Parser[Expr], op: Parser[String], b: Parser[Expr]): Parser[Expr] =
    P(a ~ op ~ b).map { case (a, op, b) => Expr.BinaryOp(op, a, b) }

  // Operators in order of precedence
  lazy val ops0: Parser[String] = P("+").!
  lazy val ops1: Parser[String] = P("*").!

  lazy val number: Parser[Expr.Number] =
    numbers.doubleLiteral.map(Expr.Number(_))

  lazy val variable: Parser[Expr.Var] =
    P("$" ~~ identifier).map(Expr.Var(_))

  lazy val funcCall: Parser[Expr] =
    P(identifier ~ "(" ~ args ~ ")").map { case (identifier, args) => Expr.FuncCall(identifier, args) }

  lazy val lambda: Parser[Expr] =
    P(identifier ~ "->" ~ expr0).map { case (identifier, body) => Expr.Lambda(identifier, body) }

  lazy val args: Parser[List[Expr]] =
    P(expr1 ~ ("," ~ args).?).map { case (head, tail) => head :: tail.getOrElse(Nil) }

  lazy val identifier: Parser[String] = (alpha ~~ alphaNum.repX(1).?).!
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
