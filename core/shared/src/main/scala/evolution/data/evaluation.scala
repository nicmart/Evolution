package evolution.data
import cats.{ Applicative, Id }
import evolution.algebra.representation.RNGRepr
import evolution.primitive.algebra.binding.interpreter.BindingAnnotator
import evolution.primitive.algebra.binding.interpreter.BindingAnnotator.{ app, fix }
import evolution.data.EvaluationContext._
import evolution.data.EvaluationContextModule._
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr
import evolution.primitive.algebra.evolution.interpreter.EvolutionAnnotator
import evolution.random.RNG

import scala.util.Random

trait EvaluationModule {
  type R[T]
  type F[T]

  // TODO it would be nice to make the seed abstract too
  def newSeed: Long
  def interpreter: Evolution[F, R]
  def materialize[T](seed: Long, fa: R[F[T]]): Iterator[T]
  def materializeConstant[T](t: R[T]): T

  final def materializeExpr[T](seed: Long, expr: Expr[F, F[T]]): Iterator[T] =
    materialize(seed, expr.run[R](interpreter))
}

private[data] object EvaluationModuleImpl extends EvaluationModule {
  override type R[T] = Evaluation[T]
  override type F[T] = RNGRepr[T]
  override def newSeed: Long = Random.nextLong()
  override def interpreter: Evolution[F, R] = EvolutionAnnotator
  override def materialize[T](seed: Long, fa: R[F[T]]): Iterator[T] = fa.evaluate.iterator(RNG(seed))
  override def materializeConstant[T](t: R[T]): T = t.evaluate
}

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

  case class App2[A, B, C](f: Evaluation[A => B => C], a: Evaluation[A], b: Evaluation[B]) extends Evaluation[C] {
    override def eval(ctx: Ctx): C = f.evaluateWith(ctx)(a.evaluateWith(ctx))(b.evaluateWith(ctx))
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
      case Lam(varName, term) => fixLambda(varName, term, ctx)
      case _                  => app(expr, fix(expr)).evaluateWith(ctx) // This should never happen
    }

    private def fixLambda(varName: String, expressionOfLambda: Evaluation[A], ctx: Ctx): A = {
      lazy val a: A =
        expressionOfLambda.evaluateWith(ctx.pushLazy(() => a, s"(r) $expressionOfLambda"))
      debug("evaluating lambda of fix", a)
    }
  }

  case class Lam[A, B](varName: String, term: Evaluation[B]) extends Evaluation[A => B] {
    @inline override def eval(ctx: Ctx): A => B =
      a => term.evaluateWith(ctx.pushStrict(a, s"LambdaArg($a) of $term"))
    override def toString: String = s"$varName -> $term"
  }

  case class Lam2[A, B, C](varA: String, varB: String, term: Evaluation[C]) extends Evaluation[A => B => C] {
    override def eval(ctx: Ctx): A => B => C =
      a =>
        b => term.evaluateWith(ctx.pushStrict(a, s"LambdaArg1($a) of $term").pushStrict(b, s"LambdaArg2($b) of $term"))
  }

  case class Value[A](getA: Ctx => A, override val toString: String = "?") extends Evaluation[A] {
    @inline override def eval(ctx: Ctx): A = getA(ctx)
  }

  implicit val applicative: Applicative[Evaluation] = new Applicative[Evaluation] {
    override def pure[A](x: A): Evaluation[A] = Value(_ => x)
    override def ap[A, B](ff: Evaluation[A => B])(fa: Evaluation[A]): Evaluation[B] =
      BindingAnnotator.app(ff, fa)
  }

  var level = -1
  var total = 0
  @inline def debug[T](message: String, t: => T): T = {
    total += 1
    level += 1
    val indent = " " * (level * 4)
    val id = Random.nextInt().toHexString.take(3)
    println(s"${indent}+++ ($id): $message")
    val result = t
    println(s"${indent}--- ($id): $message")
    level -= 1
    result
  }

  @inline def debugLine[T](message: String, t: => T): T = {
    val indent = " " * (level * 4)
    println(s"${indent}$message")
    t
  }

  @inline def xdebug[T](message: String, t: => T): T = t
}
