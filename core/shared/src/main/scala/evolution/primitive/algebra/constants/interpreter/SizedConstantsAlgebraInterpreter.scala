package evolution.primitive.algebra.constants.interpreter
import cats.MonoidK
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.Sized
import evolution.primitive.algebra.constants.ConstantsAlgebra
import evolution.primitive.algebra.evolution.parser.OrMonoid

class SizedConstantsAlgebraInterpreter[S[_]](alg: ConstantsAlgebra[S], val orMonoid: MonoidK[S])
    extends ConstantsAlgebra[Sized[S, ?]]
    with OrMonoid[S] {

  override def double(d: Double): Sized[S, Double] =
    size => if (size == 1) alg.double(d) else orMonoid.empty

  override def point(x: Sized[S, Double], y: Sized[S, Double]): Sized[S, Point] =
    size => withSize(size - 1, x, y, alg.point)

  override def add[T: Semigroup](a: Sized[S, T], b: Sized[S, T]): Sized[S, T] =
    size => withSize(size - 1, a, b, alg.add[T])

  // TODO avoid creation of impossible sizes
  private def withSize[T1, T2](n: Int, a: Sized[S, T1], b: Sized[S, T1], f: (S[T1], S[T1]) => S[T2]): S[T2] =
    orSeq(for {
      i <- (1 until n).toList
      sa = a(i)
      sb = b(n - i)
    } yield f(sa, sb))
}

trait EmptyK[F[_]] {
  def isEmpty[T](t: F[T]): Boolean
}
