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
    size => if (size == 0) alg.double(d) else orMonoid.empty

  override def point(x: Sized[S, Double], y: Sized[S, Double]): Sized[S, Point] =
    size => withSize(size - 1, x, y, alg.point)

  override def add[T: Semigroup](a: Sized[S, T], b: Sized[S, T]): Sized[S, T] =
    size => withSize(size - 1, a, b, alg.add[T])

  // TODO avoid creation of impossible sizes
  // This makes the underlying Gen create a lot of failures
  private def withSize[T1, T2](n: Int, a: Sized[S, T1], b: Sized[S, T1], f: (S[T1], S[T1]) => S[T2]): S[T2] = {
    val list = for {
      i <- (0 to n).toList
      sa = a(i)
      sb = b(n - i)
    } yield f(sa, sb)

    orSeq(list)
  }
}
