package evolution.compiler.term

import evolution.compiler.types.TypeClasses.Predicate
import cats.implicits._

private[term] case class Compilation[+T](run: CompilerState => Either[String, T]):
  def flatMap[U](f: T => Compilation[U]): Compilation[U] =
    Compilation(state => run(state).flatMap(t => f(t).run(state)))
  def map[U](f: T => U): Compilation[U] = flatMap(f andThen Compilation.pure)

private[term] object Compilation:
  def pure[T](t: T): Compilation[T] = Compilation(_ => Right(t))
  def error(message: String): Compilation[Nothing] =
    Compilation(_ => Left(message))
  def fromEither[T](either: Either[String, T]): Compilation[T] =
    either.fold(error, pure)
  def traverse[A, B](ts: List[A])(f: A => Compilation[B]): Compilation[List[B]] =
    Compilation(state => ts.traverse(t => f(t).run(state)))
  def predName(predicate: Predicate): Compilation[String] =
    Compilation(state => state.predName(predicate).toRight(s"Predicate not found: $predicate"))
  def withLocalPredicate[T](predicate: Predicate)(ft: Compilation[T]): Compilation[T] =
    Compilation(state => ft.run(state.withPredicate(predicate)))
  def withLocalPredicates[T](predicates: List[Predicate])(ft: Compilation[T]): Compilation[T] =
    Compilation(state => ft.run(state.withPredicates(predicates)))
