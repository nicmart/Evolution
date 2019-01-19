package evolution.primitive
import evolution.primitive.ast.Expr
import fastparse.all
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi._

object parser {

  lazy val parser: Parser[Expr] =
    expr ~ End

  lazy val expr: Parser[Expr] =
    P(infix(term, ops2, expr) | term)

  lazy val term: Parser[Expr] =
    P(infix(factor, ops1, term) | factor)

  lazy val factor: Parser[Expr] =
    P(("(" ~ expr ~ ")") | number | variable)

  def infix(a: Parser[Expr], op: Parser[String], b: Parser[Expr]): Parser[Expr] =
    P(a ~ op ~ b).map { case (a, op, b) => Expr.BinaryOp(op, a, b) }

  // Operators in order of precedence
  lazy val ops1: Parser[String] = P("*").!
  lazy val ops2: Parser[String] = P("+").!

  lazy val number: Parser[Expr.Number] =
    numbers.doubleLiteral.map(Expr.Number)

  lazy val variable: Parser[Expr.Var] =
    P("$" ~~ (alpha ~~ alphaNum.repX(1).?).!).map(Expr.Var)

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
