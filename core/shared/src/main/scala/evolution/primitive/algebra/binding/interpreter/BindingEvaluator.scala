package evolution.primitive.algebra.binding.interpreter
import cats.Applicative
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
      case Lambda(term) => Value(fixTerm(term.get))
      case _            => app(expr, fix(expr))
    }

  override def lambda[A, B](name: String, expr: EvaluationResult[B]): EvaluationResult[A => B] =
    Lambda(expr)

  override def app[A, B](f: EvaluationResult[A => B], a: EvaluationResult[A]): EvaluationResult[B] =
    Value(ctx => debug("evaluating app", f.get(ctx)(a.get(ctx))))

  private def fixTerm[A](expr: Ctx => A): Ctx => A =
    ctx => {
      lazy val a: A = expr(push(() => a, ctx))
      debug("evaluating fix", a)
    }
}

sealed trait EvaluationResult[T] {
  @inline def get(ctx: Ctx): T
}

object EvaluationResult {
  type Ctx = List[() => Any]
  def push[T](elem: () => T, ctx: Ctx): Ctx = elem :: ctx
  def pushStrict[T](elem: T, ctx: Ctx): Ctx = (() => elem) :: ctx

  case class Constant[A](a: A) extends EvaluationResult[A] {
    override def get(ctx: Ctx): A = a
  }

  case class Lambda[A, B](term: EvaluationResult[B]) extends EvaluationResult[A => B] {
    println("Creating Lambda")
    @inline override def get(ctx: Ctx): A => B = debug("evaluating lambda", a => term.get(pushStrict(a, ctx)))
  }
  case class Value[A](getA: Ctx => A) extends EvaluationResult[A] {
    println("Creating Value")
    @inline override def get(ctx: Ctx): A = getA(ctx)
  }

  case class Var[A](n: Int) extends EvaluationResult[A] {
    @inline override def get(ctx: Ctx): A = debug(s"evaluating var($n)", ctx(n)().asInstanceOf[A])
  }

  implicit val applicative: Applicative[EvaluationResult] = new Applicative[EvaluationResult] {
    override def pure[A](x: A): EvaluationResult[A] = Value(_ => x)
    override def ap[A, B](ff: EvaluationResult[A => B])(fa: EvaluationResult[A]): EvaluationResult[B] =
      BindingEvaluator.app(ff, fa)
  }

  def debug[T](string: String, t: T): T = {
    //println(string)
    t
  }
}
