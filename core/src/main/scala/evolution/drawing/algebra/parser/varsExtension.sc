import scala.language.higherKinds

trait Alg[F[_]] {
  def pure[A](a: A): F[A]
  def sum(a: F[Int], b: F[Int]): F[Int]
  def and(a: F[Boolean], b: F[Boolean]): F[Boolean]
}

trait Term[A] {
  def run[F[_]](alg: Alg[F]): F[A]
}

trait LetExtension[F[_, _]] {
  def var0[A, E]: F[(A, E), A]
  def varS[A, B, E](e: F[E, A]): F[(B, E), A]
  def let[E, A, B](name: String, value: F[E, A])(expr: F[(A, E), B]): F[E, B]
}

// This is unrelated with the other one
//trait AlgWithExtension[F[_,_]] {
//  def pure[A, E](a: A): F[E, A]
//  def combine[A, E](a: F[E, A], b: F[E, A]): F[E, A]
//}

trait AlgFamily[F[_, _]] {
  def get[E]: Alg[({type L[A] = F[E,A]})#L]
  def pure[E, A](a: A): F[E, A] = get.pure(a)
  def sum[E](a: F[E, Int], b: F[E, Int]): F[E, Int] = get.sum(a, b)
  def and[E](a: F[E, Boolean], b: F[E, Boolean]): F[E, Boolean] = get.and(a, b)
}

trait FullAlgebra[F[_, _]] extends LetExtension[F] {
  def alg[E]: Alg[({type L[A] = F[E,A]})#L]
  // Helper methods
  def pure[E, A](a: A): F[E, A] = alg.pure(a)
  def sum[E](a: F[E, Int], b: F[E, Int]): F[E, Int] = alg.sum(a, b)
  def and[E](a: F[E, Boolean], b: F[E, Boolean]): F[E, Boolean] = alg.and(a, b)
}

object Test {

  trait L[F[_, _]] {
    def pure[A, E](value: A): F[A, E]
  }

  trait Program[A, E] {
    def run[F[_, _]](l: L[F]): F[A, E]
  }

  val x = new Program[Int, Unit] {
    def run[F[_, _]](l: L[F]): F[Int, Unit] = l.pure(12)
  }
}

Test.x


trait TermVar[A, E] {
  def run[F[_, _]](alg: LetExtension[F]): F[A, E]
}

trait Program[E, A] {
  def run[F[_, _]](alg: FullAlgebra[F]): F[E, A]
}

val program = new Program[Unit, Int] {
  def run[F[_, _]](alg: FullAlgebra[F]): F[Unit, Int] = {
    import alg._
    let("x", pure[Unit, Int](12))(sum(var0, var0))
  }
}

"xzczc"

// How to extend interpreters to the language version with vars

// Evaluation

type Id[T] = T

trait EvaluateAlg extends Alg[Id] {
  def pure[A](a: A): A = a
  def sum(a: Int, b: Int): Int = a + b
  def and(a: Boolean, b: Boolean): Boolean = a && b
}

type Ctx[E, A] = E => A
type StringWithCtx[A, E] = List[String] => String

trait EvaluateCtxAlg[E] extends Alg[({type L[A] = E => A})#L] {
  def pure[A](a: A): E => A = _ => a
  def sum(a: E => Int, b: E => Int): E => Int = env => a(env) + b(env)
  def and(a: E => Boolean, b: E => Boolean): E => Boolean = env => a(env) && b(env)
}

trait EvaluateBindings extends LetExtension[Ctx] {
  override def var0[A, E]: Ctx[(A, E), A] =
    _._1
  override def varS[A, B, E](e: Ctx[E, A]): Ctx[(B, E), A] =
    env => e(env._2)
  override def let[E, A, B](name: String, value: Ctx[E, A])(expr: Ctx[(A, E), B]): Ctx[E, B] =
    env => expr((value(env), env))
}

object EvaluateFullAlg extends FullAlgebra[Ctx] with EvaluateBindings {
  def alg[E]: Alg[({type L[A] = E => A})#L] = new EvaluateCtxAlg[E] {}
}

program.run(EvaluateFullAlg)(())

// pretty printing
trait PrintCtxAlg[E] extends Alg[({type L[A] = StringWithCtx[A, E]})#L] {
  override def pure[A](a: A): StringWithCtx[A, E] =
    _ => a.toString
  override def sum(a: StringWithCtx[Int, E], b: StringWithCtx[Int, E]): StringWithCtx[Int, E] =
    env => s"sum(${a(env)}, ${b(env)})"
  override def and(a: StringWithCtx[Boolean, E], b: StringWithCtx[Boolean, E]): StringWithCtx[Boolean, E] =
    env => s"and(${a(env)}, ${b(env)})"
}

trait PrintBindings extends LetExtension[StringWithCtx] {
  override def var0[A, E]: StringWithCtx[(A, E), A] =
    vars => "$" + vars.headOption.getOrElse("x")
  override def varS[A, B, E](e: StringWithCtx[E, A]): StringWithCtx[(B, E), A] =
    vars => e(vars.tail)
  override def let[E, A, B](name: String, value: StringWithCtx[E, A])(expr: StringWithCtx[(A, E), B]): StringWithCtx[E, B] =
    vars => s"let($name, ${value(vars)}, ${expr(name :: vars)})"
}

object PrintFullAlg extends FullAlgebra[StringWithCtx] with PrintBindings {
  override def alg[E]: Alg[({type L[A] = StringWithCtx[A, E]})#L] =
    new PrintCtxAlg[E] {}
}

program.run(PrintFullAlg)(Nil)