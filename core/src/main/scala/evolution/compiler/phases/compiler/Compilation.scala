package evolution.compiler.phases.compiler

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import evolution.compiler.tree.TypedTree

private[compiler] final case class Compilation[+T](run: CompilerState => Either[String, T])

private[compiler] object Compilation {
  def error(message: String): Compilation[Nothing] = Compilation(_ => Left(message))
  def state: Compilation[CompilerState] = Compilation(Right.apply)
  def localState[T](state: CompilerState)(ft: Compilation[T]): Compilation[T] = Compilation(_ => ft.run(state))
  def pure[T](t: T): Compilation[T] = Compilation(_ => Right(t))
  def flatMap[A, B](fa: Compilation[A], f: A => Compilation[B]): Compilation[B] =
    Compilation(state => fa.run(state).flatMap(a => f(a).run(state)))

  def withBinding[T](name: String, tree: TypedTree)(ft: Compilation[T]): Compilation[T] =
    for {
      currentState <- state
      t <- localState[T](currentState.withBinding(name, tree))(ft)
    } yield t

  def binding(name: String): Compilation[Option[TypedTree]] = state.map(_.binding(name))

  implicit val compilationIsMonad: Monad[Compilation] = CompilationMonad

  object CompilationMonad extends Monad[Compilation] {
    override def pure[A](x: A): Compilation[A] = Compilation.pure(x)
    override def flatMap[A, B](fa: Compilation[A])(f: A => Compilation[B]): Compilation[B] = Compilation.flatMap(fa, f)
    override def tailRecM[A, B](a: A)(f: A => Compilation[Either[A, B]]): Compilation[B] = Compilation { state =>
      f(a).run(state) match {
        case Left(error)     => Left(error)
        case Right(Left(a))  => tailRecM(a)(f).run(state)
        case Right(Right(b)) => Right(b)
      }
    }
  }

}
