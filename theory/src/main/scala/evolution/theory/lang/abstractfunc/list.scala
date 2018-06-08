package evolution.theory.lang.abstractfunc

import evolution.theory.lang.abstractfunc.Types.StringConst

trait ListAlgebra[F[_]] {
  def empty[A]: F[A]
  def cons[A](head: () => A, tail: () => F[A]): F[A]
  def fix[A](f: A ~> A): F[A]
}

trait ~>[A, B] {
  def run[F[_]](alg: ListAlgebra[F]): (() => F[A]) => F[B]
}

object StreamInterpreter extends ListAlgebra[Stream] {
  override def empty[A]: Stream[A] = Stream.empty
  override def cons[A](head: () => A, tail: () => Stream[A]): Stream[A] = head() #:: tail()
  override def fix[A](f: A ~> A): Stream[A] = f.run(this)(() => fix(f))
}

object Types {
  type StringConst[A] = String
}

object StringInterpreter extends ListAlgebra[StringConst] {
  override def empty[A]: String = "Nil"
  override def cons[A](head: () => A, tail: () => String): StringConst[A] = s"${head().toString}::${tail().toString}"
  override def fix[A](f: A ~> A): String = f.run(StringInterpreter)(() => "$self")
}
