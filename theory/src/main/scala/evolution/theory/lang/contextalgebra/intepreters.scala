package evolution.theory.lang.contextalgebra

import evolution.theory.lang.higherorder.intepreters.{Ctx, StringWithContext}

object intepreters {
  type Ctx[-E, +A] = E => A
  type StringConst[-E, +A] = String
  type StringWithContext[-E, +A] = List[String] => String
  type Id[-E, +A] = A
}

class Evaluate[E] extends FullLang[E, Ctx] {
  override def int(n: Int): Ctx[E, Int] =
    _ => n
  override def bool(b: Boolean): Ctx[E, Boolean] =
    _ => b
  override def add(n: Ctx[E, Int], m: Ctx[E, Int]): Ctx[E, Int] =
    e => n(e) + m(e)
  override def ifElse[A](condition: Ctx[E, Boolean], ifTrue: Ctx[E, A], ifFalse: Ctx[E, A]): Ctx[E, A] =
    e => if(condition(e)) ifTrue(e) else ifFalse(e)
  override def var0[A]: Ctx[(A, E), A] =
    _._1
  override def let[A, B](name: String, value: Ctx[E, A])(expr: Ctx[(A, E), B]): Ctx[E, B] =
    env => expr((value(env), env))
  override def varS[A, B](e: Ctx[E, A]): Ctx[(B, E), A] =
    env => e(env._2)
}

object Evaluate {
  object Alg extends LangAlg[Ctx] {
    override def get[E]: FullLang[E, Ctx] = new Evaluate[E]
  }
}

class Serialize[E] extends FullLang[E, StringWithContext] {
  override def int(n: Int): StringWithContext[E, Int] =
    _ => n.toString
  override def bool(b: Boolean): StringWithContext[E, Boolean] =
    _ => b.toString
  override def add(n: StringWithContext[E, Int], m: StringWithContext[E, Int]): StringWithContext[E, Int] =
    ctx => s"add(${n(ctx)}, ${m(ctx)})"
  override def ifElse[A](condition: StringWithContext[E, Boolean], ifTrue: StringWithContext[E, A], ifFalse: StringWithContext[E, A]): List[String] => String =
    ctx => s"if(${condition(ctx)}, ${ifTrue(ctx)}, ${ifFalse(ctx)})"
  override def var0[A]: StringWithContext[(A, E), A] =
    ctx => "$" + ctx.headOption.getOrElse("x")
  override def let[A, B](name: String, v: StringWithContext[E, A])(e: StringWithContext[(A, E), B]): StringWithContext[E, B] =
    ctx => s"let($name, ${v(ctx)}, ${e(name :: ctx)})"
  override def varS[A, B](e: StringWithContext[E, A]): StringWithContext[(B, E), A] =
    ctx => e(ctx.tail)
}

object Serialize {
  object Alg extends LangAlg[StringWithContext] {
    override def get[E]: FullLang[E, StringWithContext] = new Serialize[E]
  }
}

