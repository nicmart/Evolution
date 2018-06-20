package evolution.theory.lang.lambda
import Lambda._

object Lambda {
  type Id[A] = A
  type ConstString[A] = String
}

trait LambdaAlg[F[_]] {
  def int(a: Int): F[Int]
  def lam[A, B](f: F[A] => F[B]): F[A => B]
  def app[A, B](f: F[A => B]): F[A] => F[B]
}

trait ListAlg[F[_], L[_]] {
  def nil[A]: L[A]
  def cons[A](head: F[A], tail: L[A]): L[A]
  def flatMap[A, B](la: L[A])(f: F[A => L[B]]): L[B]
}

trait Alg[F[_], L[_]] extends LambdaAlg[F] with ListAlg[F, L]

object ListEval extends Alg[Id, List] {
  override def nil[A]: List[A] = List.empty
  override def cons[A](head: Id[A], tail: List[A]): List[A] = head :: tail
  override def flatMap[A, B](la: List[A])(f: A => List[B]): List[B] = la.flatMap(f)
  override def int(a: Int): Int = a
  override def lam[A, B](f: A => B): A => B = f
  override def app[A, B](f: A => B): A => B = f
}

object Printer extends Alg[ConstString, ConstString] {
  override def nil[A]: String =
    "nil"
  override def cons[A](head: String, tail: String): String =
    s"$head::$tail"
  override def flatMap[A, B](la: String)(f: String): String =
    s"flatMap($la)($f)"
  override def int(a: Int): String =
    a.toString
  override def lam[A, B](f: String => String): String =
    s"x -> ${f("x")}"
  override def app[A, B](f: String): String => String =
    x => s"app($f)($x)"
}

trait TwoWayMap[F[_], G[_]] {
  def fwd[A](fa: F[A]): G[A]
  def bwd[A](ga: G[A]): F[A]
}

class FMappedAlg[F1[_], F2[_], L[_]](alg: Alg[F1, L], map: TwoWayMap[F1, F2]) extends Alg[F2, L] {
  import map._
  override def nil[A]: L[A] =
    alg.nil
  override def cons[A](head: F2[A], tail: L[A]): L[A] =
    alg.cons(bwd(head), tail)
  override def flatMap[A, B](la: L[A])(f: F2[A => L[B]]): L[B] =
    alg.flatMap(la)(bwd(f))
  override def int(a: Int): F2[Int] =
    fwd(alg.int(a))
  override def lam[A, B](f: F2[A] => F2[B]): F2[A => B] =
    fwd(alg.lam(f1 => bwd(f(fwd(f1)))))
  override def app[A, B](f: F2[A => B]): F2[A] => F2[B] =
    f2 => fwd(alg.app(bwd(f))(bwd(f2)))
}

class LMappedAlg[F[_], L1[_], L2[_]](alg: Alg[F, L1], map: TwoWayMap[L1, L2]) extends Alg[F, L2] {
  import map._
  override def nil[A]: L2[A] =
    fwd(alg.nil)
  override def cons[A](head: F[A], tail: L2[A]): L2[A] =
    fwd(alg.cons(head, bwd(tail)))
  override def flatMap[A, B](la: L2[A])(f: F[A => L2[B]]): L2[B] =
    fwd(alg.flatMap(bwd(la))(lam[A, L1[B]](fa => app(f)(fa))))
  override def int(a: Int): F[Int] = alg.int(a)
  override def lam[A, B](f: F[A] => F[B]): F[A => B] = alg.lam(f)
  override def app[A, B](f: F[A => B]): F[A] => F[B] = alg.app(f)
}
