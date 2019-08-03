package evolution.typeclass

import evolution.geometry.Point
import evolution.materialization.Iterable
import cats.kernel.Group

trait Semigroupoid[A, B, C] { self =>
  def combine(a: A, b: B): C
  final def op: Semigroupoid[B, A, C] = new Semigroupoid[B, A, C] {
    def combine(b: B, a: A): C = self.combine(a, b)
  }
}

object Semigroupoid {
  def apply[A, B, C](implicit sg: Semigroupoid[A, B, C]): Semigroupoid[A, B, C] = sg

  def fromGroup[T](group: Group[T]): Semigroupoid[T, T, T] = (a, b) => group.combine(a, b)

  object Multiplicative {
    val dblDblDbl: Semigroupoid[Double, Double, Double] = (a, b) => a * b
    val dblPointPoint: Semigroupoid[Double, Point, Point] = (a, b) => b * a
    val pointDblPoint: Semigroupoid[Point, Double, Point] = (a, b) => a * b
    val intIntInt: Semigroupoid[Int, Int, Int] = (a, b) => a * b
    val intDblDbl: Semigroupoid[Int, Double, Double] = (a, b) => a * b
    val dblIntDbl: Semigroupoid[Double, Int, Double] = (a, b) => a * b
    val intPointPoint: Semigroupoid[Int, Point, Point] = (a, b) => b * a
    val pointIntPoint: Semigroupoid[Point, Int, Point] = (a, b) => a * b
    val dblEvoDblEvoDbl: Semigroupoid[Double, Iterable[Double], Iterable[Double]] = (a, b) => Iterable.map[Double, Double](b, _ * a)
    val evoDblDblEvoDbl: Semigroupoid[Iterable[Double], Double, Iterable[Double]] = (a, b) => Iterable.map[Double, Double](a, _ * b)
    val dblEvoPointEvoPoint: Semigroupoid[Double, Iterable[Point], Iterable[Point]] = (a, b) => Iterable.map[Point, Point](b, _ * a)
    val evoPointDblEvoPoint: Semigroupoid[Iterable[Point], Double, Iterable[Point]] = (a, b) => Iterable.map[Point, Point](a, _ * b)
  }

  object Additive {
    val dblDblDbl: Semigroupoid[Double, Double, Double] = (a, b) => a + b
    val intIntInt: Semigroupoid[Int, Int, Int] = (a, b) => a + b
    val intDblDbl: Semigroupoid[Int, Double, Double] = (a, b) => a + b
    val dblIntDbl: Semigroupoid[Double, Int, Double] = (a, b) => a + b
    val pointPointPoint: Semigroupoid[Point, Point, Point] = (a, b) => a + b
    val evoDblEvoDblEvoDbl: Semigroupoid[Iterable[Double], Iterable[Double], Iterable[Double]] =
      (a, b) => Iterable.zipWithUncurried[Double, Double, Double]((x, y) => x + y)(a, b)
    val evoPointEvoPointEvoPoint: Semigroupoid[Iterable[Point], Iterable[Point], Iterable[Point]] =
    (a, b) => Iterable.zipWithUncurried[Point, Point, Point]((x, y) => x + y)(a, b)
  }
}
