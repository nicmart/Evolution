package evolution.data
import cats.{ Applicative, Id }
import evolution.algebra.representation.RNGRepr
import evolution.data.EvaluationContext._
import evolution.data.EvaluationContextModule._
import evolution.primitive.InitialInterpreterModule
import evolution.primitive.algebra.binding.interpreter.BindingEvaluator
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr
import evolution.primitive.algebra.evolution.interpreter.EvolutionEvaluator
import evolution.random.RNG

import scala.util.Random

trait EvaluationModule extends {
  type F[T]
  type Result[T]

  final val initial = new Initial[F] {}
  final type Expr[T] = initial.R[T]

  // TODO it would be nice to make the seed abstract too
  def newSeed: Long

  def interpret[T](expr: Expr[T]): Result[T]

  final def materialize[T](seed: Long, fa: Result[F[T]]): Iterator[T] = materializeWith(seed, fa, emptyCtx)
  def materializeWith[T](seed: Long, fa: Result[F[T]], ctx: Ctx): Iterator[T]
  def materializeConstant[T](t: Result[T]): T
  def materializeConstantWith[T](t: Result[T], ctx: Ctx): T

  final def materializeExpr[T](seed: Long, expr: Expr[F[T]]): Iterator[T] =
    materialize(seed, interpret(expr))

}

private[data] object EvaluationModuleImpl extends EvaluationModule with InitialInterpreterModule with Initial[RNGRepr] {
  override type Result[T] = Out[T]
  override type F[T] = RNGRepr[T]

  override def interpret[T](expr: Expr[T]): Out[T] =
    Interpreter.interpret(expr)
  override def newSeed: Long = Random.nextLong()
  override def materializeWith[T](seed: Long, fa: Result[F[T]], ctx: Ctx): Iterator[T] =
    fa(ctx).iterator(RNG(seed))
  override def materializeConstant[T](t: Result[T]): T = materializeConstantWith(t, emptyCtx)
  override def materializeConstantWith[T](t: Result[T], ctx: Ctx): T = t(ctx)
}

sealed trait Evaluation[T] {
  @inline def evaluateWith(ctx: Ctx): T = Evaluation.debug(s"$this", eval(ctx))
  @inline def evaluate: T = evaluateWith(emptyCtx)
  def map[S](f: T => S): Evaluation[S] = Evaluation.map(this, f)
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
      case Lam(varName, term) => {
        lazy val a: A = term.evaluateWith(ctx.pushLazy(() => a, s"(r) $term"))
        debug("evaluating lambda of fix", a)
      }
      case _ => ??? //app(expr, fix(expr)).evaluateWith(ctx) // This should never happen
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

  def map[A, B](ea: Evaluation[A], f: A => B): Evaluation[B] =
    ea match {
      case Constant(a, label) => Constant(f(a), label)
      case _                  => Value(ctx => f(ea.evaluateWith(ctx)))
    }

  def map2[A, B, C](ea: Evaluation[A], eb: Evaluation[B])(f: (A, B) => C): Evaluation[C] =
    (ea, eb) match {
      case (Constant(a, labelA), Constant(b, labelB)) => Constant(f(a, b), s"$labelA $labelB")
      case _                                          => Value(ctx => f(ea.evaluateWith(ctx), eb.evaluateWith(ctx)))
    }

  implicit val applicative: Applicative[Evaluation] = new Applicative[Evaluation] {
    override def pure[A](x: A): Evaluation[A] = Value(_ => x)
    override def ap[A, B](ff: Evaluation[A => B])(fa: Evaluation[A]): Evaluation[B] =
      BindingEvaluator.app(ff, fa)
  }

  var level = -1
  var total = 0
  @inline def xdebug[T](message: => String, t: => T): T = {
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

  @inline def debugLine[T](message: => String, t: => T): T = {
    val indent = " " * (level * 4)
    //println(s"${indent}$message")
    t
  }

  @inline def debug[T](message: => String, t: => T): T = t
}
