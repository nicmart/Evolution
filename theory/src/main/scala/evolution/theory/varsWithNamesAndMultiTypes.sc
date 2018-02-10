import Parsers.{Id, chooseIfTypeMatches}
import fastparse.WhitespaceApi
import fastparse.noApi.P

import scala.language.higherKinds

trait Lang[F[-_, +_]] {
  def int[E](n: Int): F[E, Int]
  def bool[E](b: Boolean): F[E, Boolean]
  def add[E](n: F[E, Int], m: F[E, Int]): F[E, Int]
  def ifElse[E, A](condition: F[E, Boolean], ifTrue: F[E, A], ifFalse: F[E, A]): F[E, A]
  def var0[E, A]: F[(A, E), A]
  def varS[E, A, B](e: F[E, A]): F[(B, E), A]
  def let[E, A, B](name: String, value: F[E, A])(expr: F[(A, E), B]): F[E, B]
}

trait Term[A] {
  def run[F[-_, +_], E](alg: Lang[F]): F[E, A]
}

trait TermE[-E, +A] {
  def run[F[-_, +_]](alg: Lang[F]): F[E, A]
}

type Ctx[-E, +A] = E => A
type StringConst[-E, +A] = String
type StringWithContext[-E, +A] = List[String] => String
type Id[-E, +A] = A

trait Test[F[_]]
new Test[Id[? ,String]]

object Evaluate extends Lang[Ctx] {
  override def int[E](n: Int): Ctx[E, Int] =
    _ => n
  override def bool[E](b: Boolean): Ctx[E, Boolean] =
    _ => b
  override def add[E](n: Ctx[E, Int], m: Ctx[E, Int]): Ctx[E, Int] =
    e => n(e) + m(e)
  override def ifElse[E, A](condition: Ctx[E, Boolean], ifTrue: Ctx[E, A], ifFalse: Ctx[E, A]): Ctx[E, A] =
    e => if(condition(e)) ifTrue(e) else ifFalse(e)
  override def var0[E, A]: Ctx[(A, E), A] =
    _._1
  override def let[E, A, B](name: String, value: Ctx[E, A])(expr: Ctx[(A, E), B]): Ctx[E, B] =
    env => expr((value(env), env))
  override def varS[E, A, B](e: Ctx[E, A]): Ctx[(B, E), A] =
    env => e(env._2)
}

object BuilderE extends Lang[TermE] {
  override def int[E](n: Int): TermE[E, Int] =
    new TermE[E, Int] { override def run[F[- _, + _]](alg: Lang[F]) = alg.int(n) }
  override def bool[E](b: Boolean): TermE[E, Boolean] =
    new TermE[E, Boolean] { override def run[F[- _, + _]](alg: Lang[F]) = alg.bool(b) }
  override def add[E](n: TermE[E, Int], m: TermE[E, Int]): TermE[E, Int] =
    new TermE[E, Int] { override def run[F[- _, + _]](alg: Lang[F]): F[E, Int] = alg.add(n.run(alg), m.run(alg)) }
  override def ifElse[E, A](condition: TermE[E, Boolean], ifTrue: TermE[E, A], ifFalse: TermE[E, A]): TermE[E, A] =
    new TermE[E, A] { override def run[F[- _, + _]](alg: Lang[F]) = alg.ifElse(condition.run(alg), ifTrue.run(alg), ifFalse.run(alg)) }
  override def var0[E, A]: TermE[(A, E), A] =
    new TermE[(A, E), A] { override def run[F[- _, + _]](alg: Lang[F]): F[(A, E), A] = alg.var0 }
  override def let[E, A, B](name: String, value: TermE[E, A])(expr: TermE[(A, E), B]): TermE[E, B] =
    new TermE[E, B] { override def run[F[- _, + _]](alg: Lang[F]): F[E, B] = alg.let(name, value.run(alg))(expr.run(alg)) }
  override def varS[E, A, B](e: TermE[E, A]): TermE[(B, E), A] =
    new TermE[(B, E), A] { override def run[F[- _, + _]](alg: Lang[F]): F[(B,E), A] = alg.varS(e.run(alg)) }
}

object Serialize extends Lang[StringWithContext] {
  override def int[E](n: Int): StringWithContext[E, Int] =
    _ => n.toString
  override def bool[E](b: Boolean): StringWithContext[E, Boolean] =
    _ => b.toString
  override def add[E](n: StringWithContext[E, Int], m: StringWithContext[E, Int]): StringWithContext[E, Int] =
    ctx => s"add(${n(ctx)}, ${m(ctx)})"
  override def ifElse[E, A](condition: StringWithContext[E, Boolean], ifTrue: StringWithContext[E, A], ifFalse: StringWithContext[E, A]) =
    ctx => s"if(${condition(ctx)}, ${ifTrue(ctx)}, ${ifFalse(ctx)})"
  override def var0[E, A]: StringWithContext[(A, E), A] =
    ctx => "$" + ctx.headOption.getOrElse("x")
  override def let[E, A, B](name: String, v: StringWithContext[E, A])(e: StringWithContext[(A, E), B]): StringWithContext[E, B] =
    ctx => s"let($name, ${v(ctx)}, ${e(name :: ctx)})"
  override def varS[E, A, B](e: StringWithContext[E, A]): StringWithContext[(B, E), A] =
    ctx => e(ctx.tail)
}

val expr1: Term[Int] = new Term[Int] {
  override def run[F[-_, +_], E](alg: Lang[F]) = {
    import alg._
    add(int(1), int(2))
  }
}

val expr2: Term[Int] = new Term[Int] {
  override def run[F[-_, +_], E](alg: Lang[F]) = {
    import alg._
    let("x", int(4))(add(var0, var0))
  }
}

val expr2e: TermE[Unit, Int] = {
  import BuilderE._
  let("x", int(4))(add(var0, var0))
}

expr1.run(Evaluate)(())
expr1.run(Serialize)
expr2.run(Evaluate)(())
expr2.run(Serialize)
expr2e.run(Serialize)
expr2e.run(Evaluate)(())


object Parsers {
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

  def chooser[F[_]]: TypeAlg[({ type X[T] = F[T] => TypeAlg[({ type Y[U] = F[U] => F[U] })#Y] })#X] =
    new TypeAlg[({ type X[T] = F[T] => TypeAlg[({ type Y[U] = F[U] => F[U] })#Y] })#X] {
      override def int: F[Int] => TypeAlg[({ type Y[U] = F[U] => F[U] })#Y] =
        fint1 => new TypeAlg[({ type Y[U] = F[U] => F[U] })#Y] {
          override def int: F[Int] => F[Int] = fint2 => fint1
          override def bool: F[Boolean] => F[Boolean] = fbool => fbool
        }
      override def bool: F[Boolean] => TypeAlg[({ type Y[U] = F[U] => F[U] })#Y] =
        fbool1 => new TypeAlg[({ type Y[U] = F[U] => F[U] })#Y] {
          override def int: F[Int] => F[Int] = fint => fint
          override def bool: F[Boolean] => F[Boolean] = fbool2 => fbool1
        }
    }

  type Id[T] = T

  /**
    * Returns u if U type is the same as T, returns t otherwise
    */
  def chooseIfTypeMatches[F[_], A: Type, B: Type](t: F[A], u: F[B]): F[A] =
    Type[A].run[({ type Y[U] = F[U] => F[U] })#Y](Type[B].run[({ type X[T] = F[T] => TypeAlg[({ type Y[U] = F[U] => F[U] })#Y] })#X](chooser[F])(u))(t)

/*
  object InitialEnc {
    sealed trait Type[T] {
      def fold[F[_]](ifInt: F[Int], ifBoolean: F[Boolean]): F[T]
    }

    object Type {

      case object Int extends Type[Int] {
        def fold[F[_]](ifInt: F[Int], ifBoolean: F[Boolean]): F[Int] = ifInt
      }

      case object Bool extends Type[Boolean] {
        def fold[F[_]](ifInt: F[Int], ifBoolean: F[Boolean]): F[Boolean] = ifBoolean
      }

      implicit val intType: Type[Int] = Int
      implicit val boolType: Type[Boolean] = Bool

      def fold[T, F[_]](ifInt: F[Int], ifBoolean: F[Boolean])(implicit ev: Type[T]): F[T] =
        ev.fold(ifInt, ifBoolean)
    }
  }
  */

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

  case class Parsers[E](int: Parser[TermE[E, Int]], bool: Parser[TermE[E, Boolean]])
    extends TypeAlg[({ type L[A] = Parser[TermE[E, A]]} )#L] {
    def pushVar[T: Type](varname: String): Parsers[(T, E)] =
      Parsers[(T, E)](
        chooseIfTypeMatches[({ type L[X] = Parser[TermE[(T, E), X]]})#L, Int, T](
          varS[E, Int, T](int),
          P (var0[E, T](varname) | varS[E, T, T](get[T]))
        ),
        chooseIfTypeMatches[({ type L[X] = Parser[TermE[(T, E), X]]})#L, Boolean, T](
          varS[E, Boolean, T](bool),
          P (var0[E, T](varname) | varS[E, T, T](get[T]))
        )
      )

    def get[T: Type]: Parser[TermE[E, T]] =
      Type[T].run[({ type L[X] = Parser[TermE[E, X]]})#L](this)
  }

  object Parsers {
    def empty[E]: Parsers[E] = Parsers(Fail, Fail)
  }

  def int[E]: Parser[TermE[E, Int]] =
    P(CharIn('0' to '9').rep(1).!.map(_.toInt)).map(BuilderE.int)

  def bool[E]: Parser[TermE[E, Boolean]] =
    P( P("true").map(_ => BuilderE.bool(true)) | P("false").map(_ => BuilderE.bool(false)) )

  val varName: Parser[String] =
    P(CharsWhileIn('a' to 'z').!)

  def add[E](vars: Parsers[E]): Parser[TermE[E, Int]] =
    function2("add", expr(vars).int, expr(vars).int).map { case (n, m) =>  BuilderE.add(n, m) }

  def ifElse[E, A: Type](vars: Parsers[E]): Parser[TermE[E, A]] =
    function3("if", expr(vars).bool, expr(vars).get[A], expr(vars).get[A]).map {
      case (cond, ifTrue, ifFalse) => BuilderE.ifElse(cond, ifTrue, ifFalse)
    }

  def var0[E, A](varName: String): Parser[TermE[(A, E), A]] =
    P("$" ~ varName).map(_ => BuilderE.var0)

  def varS[E, A, B](current: Parser[TermE[E, A]]): Parser[TermE[(B, E), A]] =
    current.map(t => BuilderE.varS(t))

  def let[E, A: Type, B: Type](vars: Parsers[E]): Parser[TermE[E, B]] =
    P(P("let" ~ "(" ~ varName ~ "," ~ expr(vars).get[A]  ~ "," ~ "").flatMap { case (name, value) =>
      expr(vars.pushVar[A](name)).get[B].map(e => BuilderE.let(name, value)(e))
    } ~ ")")

  def polymorphicExpr[E, A: Type](vars: => Parsers[E]): Parser[TermE[E, A]] =
    P( let[E, Int, A](vars) | let[E, Boolean, A](vars) | ifElse[E, A](vars) )

  def intExpr[E](vars: => Parsers[E]): Parser[TermE[E, Int]] =
    P(vars.int | int | add(vars) | polymorphicExpr[E, Int](vars) )

  def boolExpr[E](vars: => Parsers[E]): Parser[TermE[E, Boolean]] =
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

chooseIfTypeMatches[Parsers.Id, Int, Int](1, 2)
chooseIfTypeMatches[Parsers.Id, Boolean, Int](true, 2)

def evaluate[T: Parsers.Type](serializedExpression: String): T =
  Parsers.initialParser.get[T].parse(serializedExpression).get.value.run(Evaluate)(())
def reserialize[T: Parsers.Type](serializedExpression: String): String =
  Parsers.initialParser.get[T].parse(serializedExpression).get.value.run(Serialize)(Nil)

evaluate[Int]("1")
evaluate[Int]("add(1,1)")
evaluate[Int]("let(x,1,$x)")
evaluate[Int]("let(a,1,add($a,$a))")
evaluate[Int]("let(foo,7,add($foo,let(bar,5,add($bar,add($bar,add(2,3))))))")

evaluate[Int]("let(x,1,let(y,2,add($x,$y)))")
reserialize[Int]("let(x,1,let(z,2,add($x,$z)))")
evaluate[Int]("let(x,1,let(y,2,add($x,$y)))")
reserialize[Int]("let(x,1,let(y,2,add($x,$y)))")

// Scoping
evaluate[Int]("let(x,2,add($x,let(x,3,add($x,$x))))")
reserialize[Int]("let(x,2,add($x,let(x,3,add($x,$x))))")
evaluate[Int]("let(x,5,let(y,10,add($x,add($y,$x))))")

evaluate[Boolean]("true")
evaluate[Boolean]("let(x,true,let(y,false,$y))")

// Ifs
evaluate[Int]("if(true,1,0)")
evaluate[Int]("let(x,false,if($x,1,0))")


