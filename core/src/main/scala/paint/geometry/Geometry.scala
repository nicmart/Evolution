package paint.geometry

import cats.kernel.Monoid

/**
  * Created by nic on 26/11/2016.
  */
object Geometry
{
    case class Point(x: Double, y: Double) {
        def +(other: Point) = Point(x + other.x, y + other.y)
        def -(other: Point) = Point(x - other.x, y - other.y)
        def *(d: Double) = Point(x * d, y * d)
        def /(d: Double) = Point(x / d, y / d)
        def rotate(angle: Double) = {
            val (cos, sin) = (Math.cos(angle), Math.sin(angle))
            Point(
                cos * x - sin * y,
                sin * x + cos * y
            )
        }
        def norm(): Double = Math.sqrt(x * x + y * y)
        def distance(other: Point): Double = (this - other).norm()
        def versor(): Option[Point] = {
            val normValue = norm()
            if (normValue > 0) Some(this / normValue) else None
        }
        def rounded(): Point = Point(x.toInt + 0.5, y.toInt + 0.5)
    }

    object Point {
        val zero = Point(0, 0)

        def polar(radius: Double, angle: Double) = {
            val positiveRadius = Math.abs(radius)
            Point(
                positiveRadius * Math.cos(angle),
                positiveRadius * Math.sin(angle)
            )
        }

        implicit val pointMonoid = new Monoid[Point] {
            override def empty: Point = zero
            override def combine(x: Point, y: Point): Point = x + y
        }
    }



    type PlaneTransformation = Point => Point
}
