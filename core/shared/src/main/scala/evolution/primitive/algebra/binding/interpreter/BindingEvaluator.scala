package evolution.primitive.algebra.binding.interpreter
import cats.Applicative
import evolution.primitive.algebra.Ctx
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.interpreter.EvaluationResult._

object BindingEvaluator extends Binding[EvaluationResult, String] {
  override def var0[A]: EvaluationResult[A] = Var(0)

  override def shift[A](expr: EvaluationResult[A]): EvaluationResult[A] = expr match {
    case Var(n) => Var(n + 1)
    case _      => Value(ctx => debug("evaluating shift", expr.get(ctx.tail)))
  }
  override def let[A, B](name: String, value: EvaluationResult[A], expr: EvaluationResult[B]): EvaluationResult[B] =
    debug("evaluating let", app(lambda(name, expr), value))

  override def fix[A](expr: EvaluationResult[A => A]): EvaluationResult[A] =
    expr match {
      case Lambda(term, _) => Value(fixTerm(term.get))
      case _               => app(expr, fix(expr))
    }

  override def lambda[A, B](name: String, expr: EvaluationResult[B]): EvaluationResult[A => B] = {
    val ctxExpr = expr.get
    Lambda(expr, ctx => debug("evaluating lambda", a => ctxExpr((() => a) :: ctx)))
  }

  override def app[A, B](f: EvaluationResult[A => B], a: EvaluationResult[A]): EvaluationResult[B] = {
    val (fCtx, aCtx) = (f.get, a.get)
    Value(ctx => debug("evaluating app", fCtx(ctx)(aCtx(ctx))))
  }

  private def fixTerm[A](expr: Ctx[A]): Ctx[A] =
    ctx => {
      lazy val a: A = expr((() => a) :: ctx)
      debug("evaluating fix", a)
    }
}

sealed trait EvaluationResult[T] {
  def get: Ctx[T]
}

object EvaluationResult {

  case class Lambda[A, B](term: EvaluationResult[B], get: Ctx[A => B]) extends EvaluationResult[A => B] {
    println("Creating Lambda")
  }
  case class Value[A](get: Ctx[A]) extends EvaluationResult[A] {
    println("Creating Value")
  }

  case class Var[A](n: Int) extends EvaluationResult[A] {
    override def get: Ctx[A] = ctx => debug(s"evaluating var($n)", ctx(n)().asInstanceOf[A])
  }

  implicit val applicative: Applicative[EvaluationResult] = new Applicative[EvaluationResult] {
    override def pure[A](x: A): EvaluationResult[A] = Value(_ => x)
    override def ap[A, B](ff: EvaluationResult[A => B])(fa: EvaluationResult[A]): EvaluationResult[B] =
      BindingEvaluator.app(ff, fa)
  }

  def debug[T](string: String, t: T): T = {
    println(string)
    t
  }
}
