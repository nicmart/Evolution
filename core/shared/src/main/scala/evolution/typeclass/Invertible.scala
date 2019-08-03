package evolution.typeclass

import evolution.geometry.Point
import evolution.materialization.Iterable

trait Invertible[A] {
  def inverse(a: A): A
}

object Invertible {
  object Additive {
    val intInvertible: Invertible[Int] = new Invertible[Int] {
      def inverse(a: Int): Int = -a
    }

    val dblInvertible: Invertible[Double] = new Invertible[Double] {
      def inverse(a: Double): Double = -a
    }

    val pointInvertible: Invertible[Point] = new Invertible[Point] {
      def inverse(a: Point): Point = -a
    }

    val dblEvoInvertible: Invertible[Iterable[Double]] = new Invertible[Iterable[Double]] {
      def inverse(a: Iterable[Double]): Iterable[Double] = Iterable.map[Double, Double](a, -_)
    }

    val pointEvoInvertible: Invertible[Iterable[Point]] = new Invertible[Iterable[Point]] {
      def inverse(a: Iterable[Point]): Iterable[Point] = Iterable.map[Point, Point](a, -_)
    }
  }
}
