package paint.geometry

import cats.kernel.Monoid

/**
  * Created by nic on 26/11/2016.
  */
object Geometry
{
    case class Point(x: Int, y: Int) {
        def +(other: Point) = Point(x + other.x, y + other.y)
        def -(other: Point) = Point(x - other.x, y - other.y)
        def *(d: Double) = Point((x * d).toInt, (y * d).toInt)
        def *(d: Int) = Point(x * d, y * d)
        def /(d: Double) = Point((x / d).toInt, (y / d).toInt)
        def /(d: Int) = Point(x / d, y / d)
    }

    case class DoublePoint(x: Double, y: Double) {
        def +(other: DoublePoint) = DoublePoint(x + other.x, y + other.y)
        def -(other: DoublePoint) = DoublePoint(x - other.x, y - other.y)
        def *(d: Double) = DoublePoint(x * d, y * d)
        def /(d: Double) = DoublePoint(x / d, y / d)
        def rotate(angle: Double) = {
            val (cos, sin) = (Math.cos(angle), Math.sin(angle))
            DoublePoint(
                cos * x - sin * y,
                sin * x + cos * y
            )
        }
        def norm(): Double = Math.sqrt(x * x + y * y)
        def distance(other: DoublePoint): Double = (this - other).norm()
        def versor(): Option[DoublePoint] = {
            val normValue = norm()
            if (normValue > 0) Some(this / normValue) else None
        }
        def rounded(): DoublePoint = DoublePoint(x.toInt + 0.5, y.toInt + 0.5)
    }

    object DoublePoint {
        val zero = DoublePoint(0, 0)

        def polar(radius: Double, angle: Double) = {
            val positiveRadius = Math.abs(radius)
            DoublePoint(
                positiveRadius * Math.cos(angle),
                positiveRadius * Math.sin(angle)
            )
        }

        implicit val pointMonoid = new Monoid[DoublePoint] {
            override def empty: DoublePoint = zero
            override def combine(x: DoublePoint, y: DoublePoint): DoublePoint = x + y
        }
    }



    type PlaneTransformation = DoublePoint => DoublePoint
}
