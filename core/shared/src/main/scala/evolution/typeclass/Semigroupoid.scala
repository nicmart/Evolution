package evolution.typeclass

import evolution.geometry.Point
import evolution.materialization.Evolution
import cats.kernel.Group

trait Semigroupoid[A, B, C] { self =>
  def combine(a: A, b: B): C
  def combineJs(a: String, b: String): String = ???
  final def op: Semigroupoid[B, A, C] = new Semigroupoid[B, A, C] {
    def combine(b: B, a: A): C = self.combine(a, b)
  }
}

object Semigroupoid {
  type Semigroup[T] = Semigroupoid[T, T, T]
  def apply[A, B, C](implicit sg: Semigroupoid[A, B, C]): Semigroupoid[A, B, C] = sg

  def fromGroup[T](group: Group[T]): Semigroupoid[T, T, T] = (a, b) => group.combine(a, b)

  object Multiplicative {
    val dblDblDbl: Semigroupoid[Double, Double, Double] = new Semigroupoid[Double, Double, Double] {
      override def combine(a: Double, b: Double): Double = a * b
      override def combineJs(a: String, b: String): String = s"$a * $b"
    }

    val dblPointPoint: Semigroupoid[Double, Point, Point] =
      new Semigroupoid[Double, Point, Point] {
        override def combine(a: Double, b: Point): Point = b * a
        override def combineJs(a: String, b: String): String = s"b.multiply(a)"
      }

    val pointDblPoint: Semigroupoid[Point, Double, Point] = (a, b) => a * b
    val intIntInt: Semigroupoid[Int, Int, Int] = (a, b) => a * b
    val intDblDbl: Semigroupoid[Int, Double, Double] = (a, b) => a * b
    val dblIntDbl: Semigroupoid[Double, Int, Double] = (a, b) => a * b
    val intPointPoint: Semigroupoid[Int, Point, Point] = (a, b) => b * a
    val pointIntPoint: Semigroupoid[Point, Int, Point] = (a, b) => a * b

    val dblEvoDblEvoDbl: Semigroupoid[Double, Evolution[Double], Evolution[Double]] = (a, b) =>
      Evolution.map[Double, Double](b, _ * a)
    val evoDblDblEvoDbl: Semigroupoid[Evolution[Double], Double, Evolution[Double]] = (a, b) =>
      Evolution.map[Double, Double](a, _ * b)
    val dblEvoPointEvoPoint: Semigroupoid[Double, Evolution[Point], Evolution[Point]] = (a, b) =>
      Evolution.map[Point, Point](b, _ * a)
    val evoPointDblEvoPoint: Semigroupoid[Evolution[Point], Double, Evolution[Point]] = (a, b) =>
      Evolution.map[Point, Point](a, _ * b)

    val evoDblEvoDblEvoDbl: Semigroupoid[Evolution[Double], Evolution[Double], Evolution[Double]] =
      (fa, fb) => Evolution.zipWithUncurried[Double, Double, Double]((a, b) => a * b)(fa, fb)
    val evoPointEvoDblEvoPoint: Semigroupoid[Evolution[Point], Evolution[Double], Evolution[Point]] =
      (fa, fb) => Evolution.zipWithUncurried[Point, Double, Point]((a, b) => a * b)(fa, fb)
    val evoDblEvoPointEvoPoint: Semigroupoid[Evolution[Double], Evolution[Point], Evolution[Point]] =
      (fa, fb) => Evolution.zipWithUncurried[Double, Point, Point]((a, b) => b * a)(fa, fb)
  }

  object Additive {
    val dblDblDbl: Semigroupoid[Double, Double, Double] = (a, b) => a + b
    val intIntInt: Semigroupoid[Int, Int, Int] = (a, b) => a + b
    val intDblDbl: Semigroupoid[Int, Double, Double] = (a, b) => a + b
    val dblIntDbl: Semigroupoid[Double, Int, Double] = (a, b) => a + b
    val pointPointPoint: Semigroupoid[Point, Point, Point] = (a, b) => a + b
    val evoDblEvoDblEvoDbl: Semigroupoid[Evolution[Double], Evolution[Double], Evolution[Double]] =
      (a, b) => Evolution.zipWithUncurried[Double, Double, Double]((x, y) => x + y)(a, b)
    val evoPointEvoPointEvoPoint: Semigroupoid[Evolution[Point], Evolution[Point], Evolution[Point]] =
      (a, b) => Evolution.zipWithUncurried[Point, Point, Point]((x, y) => x + y)(a, b)
  }
}
