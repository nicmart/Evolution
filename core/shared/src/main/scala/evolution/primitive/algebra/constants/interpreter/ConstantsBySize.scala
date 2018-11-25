package evolution.primitive.algebra.constants.interpreter
import cats.MonoidK
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.Sized
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.evolution.parser.OrMonoid
import evolution.typeclass.VectorSpace

class ConstantsBySize[S[_], D](alg: Constants[S, D], val orMonoid: MonoidK[S])
    extends Constants[Sized[S, ?], D]
    with OrMonoid[S] {

  override def double(d: D): Sized[S, Double] =
    size => if (size == 0) alg.double(d) else orMonoid.empty

  override def point(x: Sized[S, Double], y: Sized[S, Double]): Sized[S, Point] =
    size => withSize(size - 1, x, y, alg.point)

  override def add[T: VectorSpace](a: Sized[S, T], b: Sized[S, T]): Sized[S, T] =
    size => withSize(size - 1, a, b, alg.add[T])

  override def sin(d: Sized[S, Double]): Sized[S, Double] =
    size => alg.sin(d(size - 1))

  override def cos(d: Sized[S, Double]): Sized[S, Double] =
    size => alg.cos(d(size - 1))

  override def multiply[T: VectorSpace](k: Sized[S, Double], t: Sized[S, T]): Sized[S, T] =
    size => withSize(size - 1, k, t, alg.multiply[T])

// TODO avoid creation of impossible sizes
  // This makes the underlying Gen create a lot of failures
  private def withSize[T1, T2, T3](n: Int, a: Sized[S, T1], b: Sized[S, T2], f: (S[T1], S[T2]) => S[T3]): S[T3] = {
    val list = for {
      i <- (0 to n).toList
      sa = a(i)
      sb = b(n - i)
    } yield f(sa, sb)

    orSeq(list)
  }
}
