trait Lang[Repr[_]] {
  def pure[A](a: A): Repr[A]
  def map[A, B](ra: Repr[A])(f: Repr[A] => Repr[B]): Repr[B]

}

trait TermF[A, F[_[_], _]] {
  def run[Repr[_]](lang: Lang[Repr]): F[Repr, A]
}

type ReprFunc[Repr[_], A, B] = Repr[A] => Repr[B]