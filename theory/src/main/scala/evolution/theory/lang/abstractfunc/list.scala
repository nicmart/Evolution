package evolution.theory.lang.abstractfunc

import evolution.theory.lang.abstractfunc.Types.{LazyStream, StringConst}

trait ListAlgebra[F[_]] {
  def empty[A]: F[A]
  def cons[A](head: A, tail: F[A]): F[A]
  def fix[A](f: A ~> A): F[A]
}

trait ~>[A, B] {
  def run[F[_]](alg: ListAlgebra[F]): F[A] => F[B]
}

object LazyStreamInterpreter extends ListAlgebra[LazyStream] {
  override def empty[A]: LazyStream[A] = () => Stream.empty
  override def cons[A](head: A, tail: LazyStream[A]): LazyStream[A] = () => head #:: tail()
  override def fix[A](f: A ~> A): LazyStream[A] = f.run(this)(fix(f))
}

object StreamInterpreter extends ListAlgebra[Stream] {
  override def empty[A]: Stream[A] = Stream.empty
  override def cons[A](head: A, tail: Stream[A]): Stream[A] = head #:: tail
  override def fix[A](f: ~>[A, A]): Stream[A] = f.run(LazyStreamInterpreter)(() => fix(f))()
}

object Types {
  type LazyStream[A] = () => Stream[A]
  type StringConst[A] = String
}

object StringInterpreter extends ListAlgebra[StringConst] {
  override def empty[A]: String = "Nil"
  override def cons[A](head: A, tail: String): StringConst[A] = s"$head::$tail"
  override def fix[A](f: A ~> A): String = s"fix($$self => ${f.run(StringInterpreter)("$self")})"
}
