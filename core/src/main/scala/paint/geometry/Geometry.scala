package paint.geometry

import cats.kernel.{Group, Monoid}
import paint.evolution.Evolution
import paint.evolution.Evolution.cycle

/**
  * Created by nic on 26/11/2016.
  */
object Geometry
{
    final case class Point(x: Double, y: Double) {
        def +(other: Point) = Point(x + other.x, y + other.y)
        def -(other: Point) = Point(x - other.x, y - other.y)
        def unary_-(): Point = Point(-x, -y)
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
        def angle: Double = (x, y) match {
            case (0, _) => 0
            case _ if x > 0 => Math.atan(y / x)
            case _ => Math.atan(y / x) + Math.PI
        }
        def versor(): Option[Point] = {
            val normValue = norm()
            if (normValue > 0) Some(this / normValue) else None
        }
        def rounded(): Point = Point(x.toInt + 0.5, y.toInt + 0.5)

        def inRectangle(topLeft: Point, bottomRight: Point): Boolean = {
            topLeft.x <= x && topLeft.y <= y && bottomRight.x >= x && bottomRight.y >= y
        }
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

        def pointsOnArc(n: Int, start: Point, angle: Double): IndexedSeq[Point] = {
            (0.0 until n * angle by angle).map(start.rotate(_))
        }

        def regularPolygon(edges: Int, radius: Double = 1): IndexedSeq[Point] = {
            val start = Point(0, radius)
            pointsOnArc(edges, start, 2 * Math.PI / edges)
        }

        def distanceFromRectangle(p: Point, topLeft: Point, bottomRight: Point): Double = {
            Math.sqrt(
                Math.pow(List(0, topLeft.x - p.x, p.x - bottomRight.x).max, 2) +
                Math.pow(List(0, topLeft.y - p.y, p.y - bottomRight.y).max, 2)
            )
        }

        def sequence(n: Int, from: Point, to: Point): List[Point] = {
            val step = (to - from) / n
            (0 to n).toList.map(from + step * _)
        }

        def grid(topLeft: Point, bottomRight: Point, w: Int, h: Int): List[Point] = {
            val xStep = (bottomRight.x - topLeft.x) / w
            val yStep = (bottomRight.y - topLeft.y) / h
            val points = for {
                i <- 0 to w
                x = topLeft.x + i * xStep
                j <- 0 to h
                y = topLeft.y + j * yStep
            } yield Point(x, y)
            points.toList
        }

        implicit val pointGroup = new Group[Point] {
            override def empty: Point = zero
            override def combine(x: Point, y: Point): Point = x + y
            override def inverse(point: Point): Point = -point
        }
    }



    type PlaneTransformation = Point => Point
}
