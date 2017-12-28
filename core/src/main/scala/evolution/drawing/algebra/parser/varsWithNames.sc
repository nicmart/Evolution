import fastparse.WhitespaceApi

import scala.language.higherKinds

trait Lang[F[-_, +_]] {
  def int[E](n: Int): F[E, Int]
  def add[E](n: F[E, Int], m: F[E, Int]): F[E, Int]
  def var0[E, A](name: String): F[(A, E), A]
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
type Id[-E, +A] = A

object Evaluate extends Lang[Ctx] {
  override def int[E](n: Int): Ctx[E, Int] =
    _ => n
  override def add[E](n: Ctx[E, Int], m: Ctx[E, Int]): Ctx[E, Int] =
    e => n(e) + m(e)
  override def var0[E, A](name: String): Ctx[(A, E), A] =
    _._1
  override def let[E, A, B](name: String, value: Ctx[E, A])(expr: Ctx[(A, E), B]): Ctx[E, B] =
    env => expr((value(env), env))
  override def varS[E, A, B](e: Ctx[E, A]): Ctx[(B, E), A] =
    env => e(env._2)
}

object BuilderE extends Lang[TermE] {
  override def int[E](n: Int): TermE[E, Int] =
    new TermE[E, Int] { override def run[F[- _, + _]](alg: Lang[F]) = alg.int(n) }
  override def add[E](n: TermE[E, Int], m: TermE[E, Int]): TermE[E, Int] =
    new TermE[E, Int] { override def run[F[- _, + _]](alg: Lang[F]): F[E, Int] = alg.add(n.run(alg), m.run(alg)) }
  override def var0[E, A](name: String): TermE[(A, E), A] =
    new TermE[(A, E), A] { override def run[F[- _, + _]](alg: Lang[F]): F[(A, E), A] = alg.var0(name) }
  override def let[E, A, B](name: String, value: TermE[E, A])(expr: TermE[(A, E), B]): TermE[E, B] =
    new TermE[E, B] { override def run[F[- _, + _]](alg: Lang[F]): F[E, B] = alg.let(name, value.run(alg))(expr.run(alg)) }
  override def varS[E, A, B](e: TermE[E, A]): TermE[(B, E), A] =
    new TermE[(B, E), A] { override def run[F[- _, + _]](alg: Lang[F]): F[(B,E), A] = alg.varS(e.run(alg)) }
}

object Serialize extends Lang[StringConst] {
  override def int[E](n: Int): StringConst[E, Int] =
    n.toString
  override def add[E](n: StringConst[E, Int], m: StringConst[E, Int]): StringConst[E, Int] =
    s"add($n, $m)"
  override def var0[E, A](name: String): StringConst[(A, E), A] =
    "$" + name
  override def let[E, A, B](name: String, v: StringConst[E, A])(e: StringConst[(A, E), B]): StringConst[E, B] =
    s"let($name, $v, $e)"
  override def varS[E, A, B](e: StringConst[E, A]): StringConst[(B, E), A] =
    e
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
    let("x", int(4))(add(var0("x"), var0("x")))
  }
}

val expr2e: TermE[Unit, Int] = {
  import BuilderE._
  let("x", int(4))(add(var0("x"), var0("x")))
}

val exprVarSucc: TermE[Any, Int] = {
  import BuilderE._
  let("x", int(2))(let[(Int, Any), Int, Int]("y", int(3))(add(var0("x"), varS(var0("asdasd")))))
}

expr1.run(Evaluate)(())
expr1.run(Serialize)
expr2.run(Evaluate)(())
expr2.run(Serialize)
expr2e.run(Serialize)
expr2e.run(Evaluate)(())


object Parsers {
  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(CharIn(" ", "\n", "\r").rep)
  }
  import White._
  import fastparse.noApi._

  def int[E]: Parser[TermE[E, Int]] =
    P(CharIn('0' to '9').rep(1).!.map(_.toInt)).map(BuilderE.int)

  val varName: Parser[String] =
    P(CharsWhileIn('a' to 'z').!)

  def add[E](innerParser: Parser[TermE[E, Int]]): Parser[TermE[E, Int]] =
    function2("add", innerParser, innerParser).map { case (n, m) =>  BuilderE.add(n, m) }

  def var0[E, A](varName: String): Parser[TermE[(A, E), A]] =
    P("$" ~ varName).map(_ => BuilderE.var0(varName))

  def varS[E, A](current: Parser[TermE[E, A]]): Parser[TermE[(Int, E), A]] =
    current.map(t => BuilderE.varS(t))

  def let[E](parser: Parser[TermE[E, Int]]): Parser[TermE[E, Int]] =
    P("let" ~/ "(" ~ varName ~ "," ~ parser ~ ",").flatMap { case (name, value) =>
        exprS(name, parser).map(e => BuilderE.let(name, value)(e))
    } ~ ")"
    //function3("let", varName, parser, exprS[E](varName, parser)).map { case (name, v, e) => BuilderE.let(name, v)(e) }

  def expr0[E]: Parser[TermE[E, Int]] =
    P(int | add(expr0) | let(expr0))
  def exprS[E](varName: String, curr: Parser[TermE[E, Int]]): Parser[TermE[(Int, E), Int]] =
    P(int | add(exprS(varName, curr)) | let(exprS(varName, curr)) | varS(curr) | var0(varName))


  def function1[A](funcName: String, parser: Parser[A]): Parser[A] =
    P(funcName ~ "(" ~/ parser ~ ")")
  def function2[A, B](funcName: String, parser1: Parser[A], parser2: Parser[B]): Parser[(A, B)] =
    P(funcName ~ "(" ~/ parser1 ~ "," ~ parser2 ~ ")")
  def function3[A, B, C](funcName: String, parser1: Parser[A], parser2: Parser[B], parser3: Parser[C]): Parser[(A, B, C)] =
    P(funcName ~ "(" ~/ parser1 ~ "," ~ parser2 ~ "," ~ parser3 ~ ")")
}

def evaluate(serializedExpression: String): Int =
  Parsers.expr0[Unit].parse(serializedExpression).get.value.run(Evaluate)(())
def reserialize(serializedExpression: String): String =
  Parsers.expr0[Unit].parse(serializedExpression).get.value.run(Serialize)

evaluate("let(foo,7,add($foo,let(bar,5,add($bar,add($bar, add(2,3))))))")
evaluate("let(x,1,$x)")
evaluate("let(x,1,let(y,2,add($x,$y)))")
reserialize("let(x,1,let(z,2,add($x,$z)))")
evaluate("let(x,1,let(y,2,add($x,$y)))")
reserialize("let(x,1,let(y,2,add($x,$y)))")

exprVarSucc.run(Evaluate)(())
exprVarSucc.run(Serialize)