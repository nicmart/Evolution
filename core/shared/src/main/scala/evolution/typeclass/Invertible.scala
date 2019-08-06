package evolution.typeclass

import evolution.geometry.Point
import evolution.materialization.Evolution

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

    val dblEvoInvertible: Invertible[Evolution[Double]] = new Invertible[Evolution[Double]] {
      def inverse(a: Evolution[Double]): Evolution[Double] = Evolution.map[Double, Double](a, -_)
    }

    val pointEvoInvertible: Invertible[Evolution[Point]] = new Invertible[Evolution[Point]] {
      def inverse(a: Evolution[Point]): Evolution[Point] = Evolution.map[Point, Point](a, -_)
    }
  }
}
