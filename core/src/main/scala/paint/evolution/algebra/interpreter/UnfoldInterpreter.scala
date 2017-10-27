package paint.evolution.algebra.interpreter

trait UnfoldInterpreter[S, Repr[+_]] {
  def unfold[A](state: S, repr: Repr[A]): Stream[A]
}
