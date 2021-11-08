package evolution.compiler.phases.compiler

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import evolution.compiler.tree.TypedTree

private[compiler] case class Compilation[+T] private (run: CompilerState => Either[String, T]):
  def attempt: Compilation[Either[String, T]] = Compilation(state => Right(run(state)))

private[compiler] object Compilation:
  def error(message: String): Compilation[Nothing] = Compilation(_ => Left(message))
  def state: Compilation[CompilerState] = Compilation(Right.apply)
  def localState[T](state: CompilerState)(ft: Compilation[T]): Compilation[T] = Compilation(_ => ft.run(state))
  def pure[T](t: T): Compilation[T] = Compilation(_ => Right(t))
  def flatMap[A, B](fa: Compilation[A], f: A => Compilation[B]): Compilation[B] =
    Compilation(state => fa.run(state).flatMap(a => f(a).run(state)))

  def fromEither[T](either: Either[String, T]): Compilation[T] = either.fold(error, pure)
  def withBinding[T](name: String, tree: TypedTree)(ft: Compilation[T]): Compilation[T] =
    for
      currentState <- state
      t <- localState[T](currentState.withBinding(name, tree))(ft)
    yield t

  def binding(name: String): Compilation[TypedTree] =
    state.map(_.binding(name)).map(_.toRight(s"Binding $name not found")).flatMap(fromEither)

  given Monad[Compilation] with
    override def pure[A](x: A): Compilation[A] = Compilation.pure(x)
    override def flatMap[A, B](fa: Compilation[A])(f: A => Compilation[B]): Compilation[B] = Compilation.flatMap(fa, f)
    override def tailRecM[A, B](a: A)(f: A => Compilation[Either[A, B]]): Compilation[B] = Compilation { state =>
      f(a).run(state) match
        case Left(error)     => Left(error)
        case Right(Left(a))  => tailRecM(a)(f).run(state)
        case Right(Right(b)) => Right(b)
    }
