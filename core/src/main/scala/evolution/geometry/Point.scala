package evolution.geometry

import cats.Eq
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.annotation.JSExportAll

@JSExportTopLevel("Point") @JSExportAll
final case class Point(x: Double, y: Double):
  def plus(other: Point): Point = Point(x + other.x, y + other.y)
  def +(other: Point) = plus(other)

  def minus(other: Point): Point = Point(x - other.x, y - other.y)
  def -(other: Point) = minus(other)

  def opposite: Point = Point(-x, -y)
  def unary_- : Point = opposite

  def mult(d: Double): Point = Point(x * d, y * d)
  def *(d: Double) = mult(d)

  def divide(d: Double): Point = Point(x / d, y / d)
  def /(d: Double) = divide(d)

  def rotate(angle: Double): Point =
    val (cos, sin) = (Math.cos(angle), Math.sin(angle))
    Point(
      cos * x - sin * y,
      sin * x + cos * y
    )
  def norm: Double = Math.sqrt(x * x + y * y)

  def distance(other: Point): Double = (this - other).norm

  def angle: Double = (x, y) match
    case (0, _)     => 0
    case _ if x > 0 => Math.atan(y / x)
    case _          => Math.atan(y / x) + Math.PI

  def versor: Point =
    val normValue = norm
    if normValue > 0 then this / normValue else Point.zero

  def inRectangle(topLeft: Point, bottomRight: Point): Boolean =
    topLeft.x <= x && topLeft.y <= y && bottomRight.x >= x && bottomRight.y >= y

  def isEqualTo(other: Point): Boolean = this == other

object Point:
  val zero: Point = Point(0, 0)

  @JSExportTopLevel("polar")
  def polar(radius: Double, angle: Double): Point =
    val positiveRadius = Math.abs(radius)
    Point(
      positiveRadius * Math.cos(angle),
      positiveRadius * Math.sin(angle)
    )

  implicit val pointEq: Eq[Point] = Eq.instance((p1, p2) => p1 == p2)
