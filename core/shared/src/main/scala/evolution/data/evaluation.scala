package evolution.data
import cats.Applicative
import evolution.primitive.algebra.binding.interpreter.BindingEvaluator
import evolution.primitive.algebra.binding.interpreter.BindingEvaluator.{ app, fix }
import evolution.data.EvaluationContext._
import evolution.data.EvaluationContextModule._

import scala.util.Random

sealed trait Evaluation[T] {
  @inline def evaluateWith(ctx: Ctx): T = Evaluation.debug(s"$this", eval(ctx))
  @inline def evaluate: T = evaluateWith(emptyCtx)
  @inline protected def eval(ctx: Ctx): T
}

object Evaluation {
  case class Constant[A](a: A, label: String = "") extends Evaluation[A] {
    override def eval(ctx: Ctx): A = a
    override def toString: String = if (label.nonEmpty) s"Constant($label)" else s"Constant($a)"
  }

  case class Var[A](n: Int, name: String) extends Evaluation[A] {
    @inline override def eval(ctx: Ctx): A = ctx.apply[A](n)
    override def toString: String = s"$$$name"
  }

  case class App[A, B](f: Evaluation[A => B], a: Evaluation[A]) extends Evaluation[B] {
    override def eval(ctx: Ctx): B = f.evaluateWith(ctx)(a.evaluateWith(ctx))
  }

  case class AppOfLambda[A, B](f: Lam[A, B], a: Evaluation[A]) extends Evaluation[B] {
    private val expr = f.term
    override protected def eval(ctx: Ctx): B = expr.evaluateWith(ctx.pushStrict(a.evaluateWith(ctx), a.toString))
  }

  case class App2OfLambda[A, B, C](inner: Evaluation[C], b: Evaluation[B], a: Evaluation[A]) extends Evaluation[C] {
    override protected def eval(ctx: Ctx): C =
      inner.evaluateWith(ctx.pushStrict(a.evaluateWith(ctx), a.toString).pushStrict(b.evaluateWith(ctx), b.toString))
  }

  case class Fix[A](expr: Evaluation[A => A]) extends Evaluation[A] {
    override def eval(ctx: Ctx): A = expr match {
      case Lam(_, term) => fixLambda(term, ctx)
      case _            => app(expr, fix(expr)).evaluateWith(ctx) // This should never happen
    }

    private def fixLambda(expressionOfLambda: Evaluation[A], ctx: Ctx): A = {
      lazy val a: A =
        expressionOfLambda.evaluateWith(ctx.pushLazy(() => a, s"Lambda(fix)($expressionOfLambda)"))
      debug("evaluating fix\nevaluating lambda of fix", a)
    }
  }

  case class Lam[A, B](varName: String, term: Evaluation[B]) extends Evaluation[A => B] {
    println("Creating Lambda")
    @inline override def eval(ctx: Ctx): A => B =
      a => term.evaluateWith(ctx.pushStrict(a, s"LambdaArg($a) of $term"))
    override def toString: String = s"lambda($varName -> $term)"
  }
  case class Value[A](getA: Ctx => A, override val toString: String = "?") extends Evaluation[A] {
    @inline override def eval(ctx: Ctx): A = getA(ctx)
  }

  implicit val applicative: Applicative[Evaluation] = new Applicative[Evaluation] {
    override def pure[A](x: A): Evaluation[A] = Value(_ => x)
    override def ap[A, B](ff: Evaluation[A => B])(fa: Evaluation[A]): Evaluation[B] =
      BindingEvaluator.app(ff, fa)
  }

  var level = -1
  @inline def debug[T](message: String, t: => T): T = {
    level += 1
    val indent = " " * (level * 4)
    val id = Random.nextInt().toHexString.take(3)
    println(s"${indent}+ ($id): $message")
    val result = t
    println(s"${indent}- ($id): $message")
    level -= 1
    result
  }
  @inline def xdebug[T](message: String, t: => T): T = t
}
