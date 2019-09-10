package evolution.geometry

import cats.Eq
import cats.kernel.Group
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("Point")
final case class Point(x: Double, y: Double) {
  def +(other: Point) = Point(x + other.x, y + other.y)
  def -(other: Point) = Point(x - other.x, y - other.y)
  def unary_-(): Point = Point(-x, -y)
  def *(d: Double) = Point(x * d, y * d)
  def /(d: Double) = Point(x / d, y / d)
  def rotate(angle: Double): Point = {
    val (cos, sin) = (Math.cos(angle), Math.sin(angle))
    Point(
      cos * x - sin * y,
      sin * x + cos * y
    )
  }
  def norm: Double = Math.sqrt(x * x + y * y)
  def distance(other: Point): Double = (this - other).norm
  def angle: Double = (x, y) match {
    case (0, _)     => 0
    case _ if x > 0 => Math.atan(y / x)
    case _          => Math.atan(y / x) + Math.PI
  }

  def versor: Point = {
    val normValue = norm
    if (normValue > 0) this / normValue else Point.zero
  }

  def rounded(): Point = Point(x.toInt + 0.5, y.toInt + 0.5)

  def inRectangle(topLeft: Point, bottomRight: Point): Boolean = {
    topLeft.x <= x && topLeft.y <= y && bottomRight.x >= x && bottomRight.y >= y
  }
}

object Point {
  val zero = Point(0, 0)

  def polar(radius: Double, angle: Double): Point = {
    val positiveRadius = Math.abs(radius)
    Point(
      positiveRadius * Math.cos(angle),
      positiveRadius * Math.sin(angle)
    )
  }

  def sequence(n: Int, from: Point, to: Point): List[Point] = {
    val step = (to - from) / n
    (0 to n).toList.map(from + step * _)
  }

  implicit val pointGroup: Group[Point] = new Group[Point] {
    override def empty: Point = zero
    override def combine(x: Point, y: Point): Point = x + y
    override def inverse(point: Point): Point = -point
  }

  implicit val pointEq: Eq[Point] = Eq.instance((p1, p2) => p1 == p2)
}
