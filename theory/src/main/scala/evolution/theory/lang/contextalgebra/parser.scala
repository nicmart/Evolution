package evolution.theory.lang.contextalgebra

import evolution.theory.lang.contextalgebra.parser.expr
import fastparse.{WhitespaceApi, noApi}

object parser {
  // "parseable types"

  trait TypeAlg[F[_]] {
    def int: F[Int]
    def bool: F[Boolean]
  }

  object TypeAlg {
    def apply[F[_]](intF: F[Int], boolF: F[Boolean]): TypeAlg[F] = new TypeAlg[F] {
      override def int: F[Int] = intF
      override def bool: F[Boolean] = boolF
    }
  }

  trait TypeAlgUndef[F[_]] extends TypeAlg[F] {
    def undefined[T]: F[T]
  }

  type PairTypeAlg[F[_, _]] = TypeAlg[λ[X => TypeAlg[λ[Y => F[X, Y]]]]]

  object PairTypeAlg {
    def apply[F[_, _]](
      intInt: F[Int, Int],
      intBool: F[Int, Boolean],
      boolInt: F[Boolean, Int],
      boolBool: F[Boolean, Boolean]
    ): PairTypeAlg[F] =
      TypeAlg[λ[X => TypeAlg[F[X, ?]]]](
        TypeAlg[F[Int, ?]](intInt, intBool),
        TypeAlg[F[Boolean, ?]](boolInt, boolBool)
      )
  }

  trait Type[A] {
    def run[F[_]](alg: TypeAlg[F]): F[A]
  }

  trait TypeUndef[A] {
    def run[F[_]](alg: TypeAlgUndef[F]): F[A]
  }

  trait PairType[A, B] {
    def run[F[_, _]](alg: PairTypeAlg[F]): F[A, B]
  }

  object PairType {
    def get[A: Type, B: Type]: PairType[A, B] = new PairType[A, B] {
      def run[F[_, _]](alg: PairTypeAlg[F]): F[A, B] =
        Type[B].run[F[A, ?]](Type[A].run[λ[X => TypeAlg[λ[Y => F[X, Y]]]]](alg))
    }
  }

  object Type {
    def apply[A](implicit t: Type[A]): Type[A] = t
  }

  object TypeUndef {
    def apply[A](implicit t: TypeUndef[A]): TypeUndef[A] = t
  }

  implicit val intType: Type[Int] = new Type[Int] {
    def run[F[_]](alg: TypeAlg[F]): F[Int] = alg.int
  }

  implicit val boolType: Type[Boolean] = new Type[Boolean] {
    def run[F[_]](alg: TypeAlg[F]): F[Boolean] = alg.bool
  }

  implicit def undefForDefType[T](implicit mytype: Type[T]): TypeUndef[T] = new TypeUndef[T] {
    def run[F[_]](alg: TypeAlgUndef[F]): F[T] = mytype.run(alg)
  }

  implicit def undefType[T]: TypeUndef[T] = new  TypeUndef[T] {
    def run[F[_]](alg: TypeAlgUndef[F]): F[T] = alg.undefined[T]
  }

  def chooser[F[_]]: TypeAlg[λ[T => F[T] => TypeAlg[λ[U => F[U] => F[U]]]]] =
    new TypeAlg[λ[T => (F[T] => TypeAlg[λ[U => F[U] => F[U]]])]] {
      override def int: F[Int] => TypeAlg[λ[U => F[U] => F[U]]] =
        fint1 => new TypeAlg[λ[U => F[U] => F[U]]] {
          override def int: F[Int] => F[Int] = fint2 => fint1
          override def bool: F[Boolean] => F[Boolean] = fbool => fbool
        }
      override def bool: F[Boolean] => TypeAlg[λ[U => F[U] => F[U]]] =
        fbool1 => new TypeAlg[λ[U => F[U] => F[U]]] {
          override def int: F[Int] => F[Int] = fint => fint
          override def bool: F[Boolean] => F[Boolean] = fbool2 => fbool1
        }
    }

  type Id[T] = T

  /**
    * Returns u if U type is the same as T, returns t otherwise
    */
  def choose[F[_], A: Type, B: Type](t: F[A], u: F[B]): F[A] =
    Type[A].run[λ[U => F[U] => F[U]]](Type[B].run[λ[T => F[T] => TypeAlg[λ[U => (F[U] => F[U])]]]](chooser[F])(u))(t)

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
    def pushVar[T: Type](varName: String): Parsers[(T, E)] = {
      type F[A, B] = Parser[Term[(A, E), B]]
      val pushedParsers = PairTypeAlg[F](
        P(var0[E, Int](varName) | varS[E, Int, Int](int)),
        varS[E, Boolean, Int](bool),
        varS[E, Int, Boolean](int),
        P(var0[E, Boolean](varName) | varS[E, Boolean, Boolean](bool))
      )
      Parsers(
        PairType.get[T, Int].run[F](pushedParsers),
        PairType.get[T, Boolean].run[F](pushedParsers)
      )
    }

    def get[T: Type]: Parser[Term[E, T]] =
      Type[T].run[λ[X => Parser[Term[E, X]]]](this)

    def getOrFail[T]: Parser[Term[E, T]] =
      TypeUndef[T].run(new TypeAlgUndef[λ[X => Parser[Term[E, X]]]] {
        def undefined[S]: Parser[Term[E, S]] = Fail
        def int: Parser[Term[E, Int]] = this.int
        def bool: Parser[Term[E, Boolean]] = this.bool
      })
  }

  object Parsers {
    def empty[E]: Parsers[E] = Parsers(Fail, Fail)
  }

  type ParserOf[E, A] = Parser[Term[E, A]]

  class ParserAlg[E](vars: Parsers[E]) extends FullLang[E, ParserOf] {
    private val builder: FullLang[E, Term] = Builder.Alg.get[E]
    def int(n: Int): ParserOf[E, Int] =
      P(CharIn('0' to '9').rep(1).!.map(_.toInt)).map(builder.int)
    def bool(b: Boolean): ParserOf[E, Boolean] =
      P( P("true").map(_ => builder.bool(true)) | P("false").map(_ => builder.bool(false)) )
    def add(n: ParserOf[E, Int], m: ParserOf[E, Int]): ParserOf[E, Int] =
      function2("add", expr.int, expr.int).map { case (nn, mm) =>  builder.add(nn, mm) }
    def ifElse[A](condition: ParserOf[E, Boolean], ifTrueParser: ParserOf[E, A], ifFalseParser: ParserOf[E, A]): ParserOf[E, A] =
      function3("if", condition, ifTrueParser, ifFalseParser).map {
        case (cond, ifTrue, ifFalse) => builder.ifElse(cond, ifTrue, ifFalse)
      }
    def var0[A]: ParserOf[(A, E), A] =
      P("$" ~ varName).map(_ => builder.var0)
    def varS[A, B](e: ParserOf[E, A]): ParserOf[(B, E), A] =
      e.map(t => builder.varS(t))
    def let[A, B](name: String, value: ParserOf[E, A])(expr: ParserOf[(A, E), B]): ParserOf[E, B] =
      P(P("let" ~ "(" ~ name ~ "," ~ value  ~ "," ~ "").flatMap { v =>
        expr.map(e => builder.let(name, v)(e))
      } ~ ")")

    private def expr: Parsers[E] = Parsers[E](
      intExpr,
      boolExpr
    )

    private def polymorphicExpr[A: Type]: Parser[Term[E, A]] =
      P( vars.get[A] | let[Int, A] | let[Boolean, A] | ifElse[A] )

    private def intExpr: Parser[Term[E, Int]] =
      P(int | add | polymorphicExpr[E, Int] )

    private def boolExpr: Parser[Term[E, Boolean]] =
      P(bool | polymorphicExpr[E, Boolean] )
  }

  def int[E]: Parser[Term[E, Int]] =
    P(CharIn('0' to '9').rep(1).!.map(_.toInt)).map(Builder.int)

  def bool[E]: Parser[Term[E, Boolean]] =
    P( P("true").map(_ => builder.bool(true)) | P("false").map(_ => builder.bool(false)) )

  val varName: Parser[String] =
    P(CharsWhileIn('a' to 'z').!)

  def add[E]: Parser[Term[E, Int]] =
    function2("add", expr(vars).int, expr(vars).int).map { case (n, m) =>  Builder.add(n, m) }

  def ifElse[E, A: Type]: Parser[Term[E, A]] =
    function3("if", expr(vars).bool, expr(vars).get[A], expr(vars).get[A]).map {
      case (cond, ifTrue, ifFalse) => Builder.ifElse(cond, ifTrue, ifFalse)
    }

  def var0[E, A](varName: String): Parser[Term[(A, E), A]] =
    P("$" ~ varName).map(_ => Builder.var0)

  def varS[E, A, B](current: Parser[Term[E, A]]): Parser[Term[(B, E), A]] =
    current.map(t => Builder.varS(t))

  def let[E, A: Type, B: Type]: Parser[Term[E, B]] =
    P(P("let" ~ "(" ~ varName ~ "," ~ expr(vars).get[A]  ~ "," ~ "").flatMap { case (name, value) =>
      expr(vars.pushVar[A](name)).get[B].map(e => Builder.let(name, value)(e))
    } ~ ")")

  def polymorphicExpr[E, A: Type]: Parser[Term[E, A]] =
    P( vars.get[A] | let[E, Int, A] | let[E, Boolean, A] | ifElse[E, A] )

  def intExpr[E]: Parser[Term[E, Int]] =
    P(int | add | polymorphicExpr[E, Int] )

  def boolExpr[E]: Parser[Term[E, Boolean]] =
    P(bool | polymorphicExpr[E, Boolean] )

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

