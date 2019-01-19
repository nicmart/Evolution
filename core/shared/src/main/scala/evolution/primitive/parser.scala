package evolution.primitive
import fastparse.noApi.{ P, Parser }
import evolution.primitive.algebra.parser.ParserConfig.White._
import evolution.primitive.ast.Expr
import fastparse.{ all, core }
import fastparse.noApi._

object parser {

  lazy val expr: Parser[Expr] =
    P(double | term) ~ End

  lazy val term: Parser[Expr] =
    P(factor)

  lazy val factor: Parser[Expr] =
    P(("(" ~ expr ~ ")") | double | variable)

  lazy val double: Parser[Expr.Dbl] =
    numbers.doubleLiteral.map(Expr.Dbl)

  lazy val variable: Parser[Expr.Var] =
    P("$" ~~ (alpha ~~ alphaNum.repX(1).?).!).map(Expr.Var)

  lazy val alpha: Parser[Unit] = CharIn('a' to 'z') | CharIn('A' to 'Z')
  lazy val alphaNum: Parser[Unit] = CharIn('0' to '9') | alpha

  object numbers {

    lazy val digit: Parser[Unit] =
      P(CharIn('0' to '9'))

    lazy val floatDigits: Parser[Unit] =
      P(digit.rep ~~ "." ~~ digit.repX(1))

    lazy val doubleLiteral: Parser[Double] =
      P("-".? ~~ (floatDigits | digit.repX(1)) ~~ exp.?).!.map(_.toDouble)

    lazy val exp: Parser[Unit] = P(CharIn("Ee") ~~ CharIn("+\\-").? ~~ digit.repX(1))
  }
}
