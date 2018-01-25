import fastparse.WhitespaceApi

import scala.language.higherKinds

trait Lang[F[-_, +_]] {
  def int[E](n: Int): F[E, Int]
  def bool[E](b: Boolean): F[E, Boolean]
  def add[E](n: F[E, Int], m: F[E, Int]): F[E, Int]
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

object Evaluate extends Lang[Ctx] {
  override def int[E](n: Int): Ctx[E, Int] =
    _ => n
  override def bool[E](b: Boolean): Ctx[E, Boolean] =
    _ => b
  override def add[E](n: Ctx[E, Int], m: Ctx[E, Int]): Ctx[E, Int] =
    e => n(e) + m(e)
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

  def int[E]: Parser[TermE[E, Int]] =
    P(CharIn('0' to '9').rep(1).!.map(_.toInt)).map(BuilderE.int)

  def bool[E]: Parser[TermE[E, Boolean]] =
    P( P("true").map(_ => BuilderE.bool(true)) | P("false").map(_ => BuilderE.bool(false)) )

  val varName: Parser[String] =
    P(CharsWhileIn('a' to 'z').!)

  def add[E](innerParser: Parser[TermE[E, Int]]): Parser[TermE[E, Int]] =
    function2("add", innerParser, innerParser).map { case (n, m) =>  BuilderE.add(n, m) }

  def var0[E, A](varName: String): Parser[TermE[(A, E), A]] =
    P("$" ~ varName).map(_ => BuilderE.var0)

  def varS[E, A](current: Parser[TermE[E, A]]): Parser[TermE[(Int, E), A]] =
    current.map(t => BuilderE.varS(t))

  def let[E, A, B](pa: Parser[TermE[E, A]], pb: Parser[TermE[E, B]]): Parser[TermE[E, B]] =
    P("let" ~/ "(" ~ varName ~/ "," ~ pa ~/ "," ~ "").flatMap { case (name, value) =>
        exprS(name, pb).map(e => BuilderE.let(name, value)(e))
    } ~ ")"
    //function3("let", varName, parser, exprS[E](varName, parser)).map { case (name, v, e) => BuilderE.let(name, v)(e) }

  def expr[E, A](current: => Parser[TermE[E, A]]): Parser[TermE[E, A]] =
    whitespaceWrap(P(int | add(current) | let(current)))
  def exprS[E, A, B](varName: String, curr: Parser[TermE[E, A]]): Parser[TermE[(A, E), A]] =
    whitespaceWrap(P(expr(exprS(varName, curr)) | varS(curr) | var0(varName)))

  def whitespaceWrap[T](p: Parser[T]): Parser[T] =
    P(Config.whitespaces ~ p ~ Config.whitespaces)

  def function1[A](funcName: String, parser: Parser[A]): Parser[A] =
    P(funcName ~ "(" ~/ parser ~ ")")
  def function2[A, B](funcName: String, parser1: Parser[A], parser2: Parser[B]): Parser[(A, B)] =
    P(funcName ~ "(" ~/ parser1 ~ "," ~ parser2 ~ ")")
  def function3[A, B, C](funcName: String, parser1: Parser[A], parser2: Parser[B], parser3: Parser[C]): Parser[(A, B, C)] =
    P(funcName ~ "(" ~/ parser1 ~ "," ~ parser2 ~ "," ~ parser3 ~ ")")

  def initialParser: Parser[TermE[Unit, Int]] = expr(initialParser)
}

def evaluate(serializedExpression: String): Int =
  Parsers.initialParser.parse(serializedExpression).get.value.run(Evaluate)(())
def reserialize(serializedExpression: String): String =
  Parsers.initialParser.parse(serializedExpression).get.value.run(Serialize)(Nil)

evaluate("let(foo,7,add($foo,let(bar,5,add($bar,add($bar, add(2,3))))))")
evaluate("let(x,1,$x)")
evaluate("let(x,1,let(y,2,add($x,$y)))")
reserialize("let(x,1,let(z,2,add($x,$z)))")
evaluate("let(x,1,let(y,2,add($x,$y)))")
reserialize("let(x,1,let(y,2,add($x,$y)))")


val exprVarSucc: TermE[Any, Int] = {
  import BuilderE._
  let("x", int(2))(let[(Int, Any), Int, Int]("y", int(3))(add(var0, varS(var0))))
}

exprVarSucc.run(Evaluate)(())

exprVarSucc.run(Serialize)(Nil)

// Scoping
evaluate("let(x,2,add($x, let(x, 3, add($x, $x))))")

evaluate(exprVarSucc.run(Serialize)(Nil))
reserialize(exprVarSucc.run(Serialize)(Nil))

evaluate("let(x, 5, let(y, 10, add($x, add($y, $x))))")


