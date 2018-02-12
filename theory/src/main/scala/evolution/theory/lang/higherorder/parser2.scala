package evolution.theory.lang.higherorder

import fastparse.WhitespaceApi

/**
  * This is the same parser, but it uses a different "choosing" strategy, with implicits instead than
  * an interpreter on the Type algebra.
  */
object parser2 {
  // "parseable types"

  trait TypeAlg[F[_]] {
    def int: F[Int]
    def bool: F[Boolean]
  }

  trait Type[A] {
    def run[F[_]](alg: TypeAlg[F]): F[A]
  }

  object Type {
    def apply[A](implicit t: Type[A]): Type[A] = t
  }

  implicit val intType: Type[Int] = new Type[Int] {
    def run[F[_]](alg: TypeAlg[F]): F[Int] = alg.int
  }

  implicit val boolType: Type[Boolean] = new Type[Boolean] {
    def run[F[_]](alg: TypeAlg[F]): F[Boolean] = alg.bool
  }

  type Id[T] = T

  trait Chooser[T, U] {
      def choose(t: T, u: U): T
      def chooseF[F[_]](t: F[T], u: F[U]): F[T]
}

  trait ChooseFirst[T, U] extends Chooser[T, U] {
    def choose(t: T, u: U): T = t
    def chooseF[F[_]](t: F[T], u: F[U]): F[T] = t
  }

  trait ChooseSecond[T] extends ChooseFirst[T, T] {
    override def choose(t: T, u: T): T = u
    override def chooseF[F[_]](t: F[T], u: F[T]): F[T] = u
  }

  object Chooser {
    implicit def chooseFirst[T, U]: ChooseFirst[T, U] = new ChooseFirst[T, U] {}
    implicit def chooseSecond[T]: ChooseSecond[T] = new ChooseSecond[T] {}
  }

  def choose[T, U](t: T, u: U)(implicit c: Chooser[T, U]): T = c.choose(t, u)
  def chooseF[F[_], T, U](t: F[T], u: F[U])(implicit c: Chooser[T, U]): F[T] = c.chooseF(t, u)

  object Config {
    import fastparse.all._
    val whitespaces = CharIn(" ", "\n", "\r").rep
  }
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(Config.whitespaces)
  }
  import White._
  import fastparse.noApi._

  case class Parsers[E](int: Parser[Term[E, Int]], bool: Parser[Term[E, Boolean]])
    extends TypeAlg[λ[X => Parser[Term[E, X]]]] {
    def pushVar[T: Type](varname: String)(implicit c1: Chooser[Int, T], c2: Chooser[Boolean, T]): Parsers[(T, E)] =
      Parsers[(T, E)](
        c1.chooseF[λ[X => Parser[Term[(T, E), X]]]](
          varS[E, Int, T](int),
          P (var0[E, T](varname) | varS[E, T, T](get[T]))
        ),
        c2.chooseF[λ[X => Parser[Term[(T, E), X]]]](
          varS[E, Boolean, T](bool),
          P (var0[E, T](varname) | varS[E, T, T](get[T]))
        )
      )

    def get[T: Type]: Parser[Term[E, T]] =
      Type[T].run[λ[X => Parser[Term[E, X]]]](this)
  }

  object Parsers {
    def empty[E]: Parsers[E] = Parsers(Fail, Fail)
  }

  def int[E]: Parser[Term[E, Int]] =
    P(CharIn('0' to '9').rep(1).!.map(_.toInt)).map(Builder.int)

  def bool[E]: Parser[Term[E, Boolean]] =
    P( P("true").map(_ => Builder.bool(true)) | P("false").map(_ => Builder.bool(false)) )

  val varName: Parser[String] =
    P(CharsWhileIn('a' to 'z').!)

  def add[E](vars: Parsers[E]): Parser[Term[E, Int]] =
    function2("add", expr(vars).int, expr(vars).int).map { case (n, m) =>  Builder.add(n, m) }

  def ifElse[E, A: Type](vars: Parsers[E]): Parser[Term[E, A]] =
    function3("if", expr(vars).bool, expr(vars).get[A], expr(vars).get[A]).map {
      case (cond, ifTrue, ifFalse) => Builder.ifElse(cond, ifTrue, ifFalse)
    }

  def var0[E, A](varName: String): Parser[Term[(A, E), A]] =
    P("$" ~ varName).map(_ => Builder.var0)

  def varS[E, A, B](current: Parser[Term[E, A]]): Parser[Term[(B, E), A]] =
    current.map(t => Builder.varS(t))

  def let[E, A: Type, B: Type](vars: Parsers[E])(implicit c1: Chooser[Int, A], c2: Chooser[Boolean, A]): Parser[Term[E, B]] =
    P(P("let" ~ "(" ~ varName ~ "," ~ expr(vars).get[A]  ~ "," ~ "").flatMap { case (name, value) =>
      expr(vars.pushVar[A](name)).get[B].map(e => Builder.let(name, value)(e))
    } ~ ")")

  def polymorphicExpr[E, A: Type](vars: => Parsers[E]): Parser[Term[E, A]] =
    P( let[E, Int, A](vars) | let[E, Boolean, A](vars) | ifElse[E, A](vars) )

  def intExpr[E](vars: => Parsers[E]): Parser[Term[E, Int]] =
    P(vars.int | int | add(vars) | polymorphicExpr[E, Int](vars) )

  def boolExpr[E](vars: => Parsers[E]): Parser[Term[E, Boolean]] =
    P(vars.bool | bool | polymorphicExpr[E, Boolean](vars) )

  def whitespaceWrap[T](p: Parser[T]): Parser[T] =
    P(Config.whitespaces ~ p ~ Config.whitespaces)

  def function1[A](funcName: String, parser: Parser[A]): Parser[A] =
    P(funcName ~ "(" ~ parser ~ ")")
  def function2[A, B](funcName: String, parser1: Parser[A], parser2: Parser[B]): Parser[(A, B)] =
    P(funcName ~ "(" ~ parser1 ~ "," ~ parser2 ~ ")")
  def function3[A, B, C](funcName: String, parser1: Parser[A], parser2: Parser[B], parser3: Parser[C]): Parser[(A, B, C)] =
    P(funcName ~ "(" ~ parser1 ~ "," ~ parser2 ~ "," ~ parser3 ~ ")")

  def expr[E](vars: => Parsers[E]): Parsers[E] = Parsers[E](
    intExpr[E](vars),
    boolExpr[E](vars)
  )

  val initialParser: Parsers[Unit] = expr(Parsers.empty)
}

