package evolution.data
import cats.Applicative
import evolution.primitive.algebra.binding.interpreter.BindingEvaluator
import evolution.primitive.algebra.binding.interpreter.BindingEvaluator.{ app, fix }
import evolution.data.EvaluationContext._
import evolution.data.EvaluationContextModule._

import scala.util.Random

sealed trait Result[T] {
  @inline def evaluate(ctx: Ctx): T =
    Result.debug(s"Evaluating $this", eval(ctx))
  //evaluate(ctx)
  def evaluate: T = evaluate(emptyCtx)
  @inline protected def eval(ctx: Ctx): T
}

object Result {
  case class Constant[A](a: A, label: String = "") extends Result[A] {
    override def eval(ctx: Ctx): A = a
    override def toString: String = if (label.nonEmpty) s"Constant($label)" else s"Constant($a)"
  }

  case class Var[A](n: Int, name: String) extends Result[A] {
    @inline override def eval(ctx: Ctx): A = ctx.apply[A](n)
    override def toString: String = s"$$$name"
  }

  case class App[A, B](f: Result[A => B], a: Result[A]) extends Result[B] {
    override def eval(ctx: Ctx): B = f.evaluate(ctx)(a.evaluate(ctx))
  }

  case class AppOfLambda[A, B](f: Lam[A, B], a: Result[A]) extends Result[B] {
    private val expr = f.term
    override protected def eval(ctx: Ctx): B = expr.evaluate(pushStrict(a.evaluate(ctx), ctx))
  }

  case class App2OfLambda[A, B, C](inner: Result[C], b: Result[B], a: Result[A]) extends Result[C] {
    override protected def eval(ctx: Ctx): C =
      inner.evaluate(pushStrict(b.evaluate(ctx), pushStrict(a.evaluate(ctx), ctx)))
  }

  case class Fix[A](expr: Result[A => A]) extends Result[A] {
    override def eval(ctx: Ctx): A = expr match {
      case Lam(_, term) => fixTerm(term.evaluate)(ctx)
      case _            => app(expr, fix(expr)).evaluate(ctx)
    }

    private def fixTerm(expr: Ctx => A): Ctx => A =
      ctx => {
        lazy val a: A = expr(pushLazy(() => a, ctx))
        debug("evaluating fix\nevaluating lambda of fix", a)
      }
  }

  case class Lam[A, B](varName: String, term: Result[B]) extends Result[A => B] {
    println("Creating Lambda")
    @inline override def eval(ctx: Ctx): A => B =
      a => term.evaluate(pushStrict(a, ctx))
    override def toString: String = s"lambda($varName -> $term)"
  }
  case class Value[A](getA: Ctx => A, override val toString: String = "?") extends Result[A] {
    @inline override def eval(ctx: Ctx): A = getA(ctx)
  }

  implicit val applicative: Applicative[Result] = new Applicative[Result] {
    override def pure[A](x: A): Result[A] = Value(_ => x)
    override def ap[A, B](ff: Result[A => B])(fa: Result[A]): Result[B] =
      BindingEvaluator.app(ff, fa)
  }

  var level = -1
  @inline def debug[T](message: String, t: => T): T = {
    level += 1
    val indent = " " * (level * 4)
    val id = Random.nextInt().toHexString.take(6)
    println(s"${indent}START ($id): $message")
    val result = t
    println(s"${indent}END   ($id): $message")
    level -= 1
    result
  }
  @inline def xdebug[T](message: String, t: => T): T = {
    t
  }
}
