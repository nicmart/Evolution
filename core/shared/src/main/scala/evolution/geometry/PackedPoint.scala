package evolution.geometry
import java.lang.Float._

import cats.kernel.Group

final case class PackedPoint(packed: Long) extends AnyVal {
  def x: Float = intBitsToFloat(packed.toInt)
  def y: Float = intBitsToFloat((packed >>> 32).toInt)

  def +(other: PackedPoint) = PackedPoint(x + other.x, y + other.y)
  def -(other: PackedPoint) = PackedPoint(x - other.x, y - other.y)
  def unary_-(): PackedPoint = PackedPoint(-x, -y)
  def *(d: Double) = PackedPoint(x * d, y * d)
  def /(d: Double) = PackedPoint(x / d, y / d)
  def rotate(angle: Double): PackedPoint = {
    val (cos, sin) = (Math.cos(angle), Math.sin(angle))
    PackedPoint(
      cos * x - sin * y,
      sin * x + cos * y
    )
  }
  def norm(): Double = Math.sqrt(x * x + y * y)
  def distance(other: PackedPoint): Double = (this - other).norm()
  def angle: Double = (x, y) match {
    case (0, _) => 0
    case _ if x > 0 => Math.atan(y / x)
    case _ => Math.atan(y / x) + Math.PI
  }
  def versor(): Option[PackedPoint] = {
    val normValue = norm()
    if (normValue > 0) Some(this / normValue) else None
  }
  def rounded(): PackedPoint = PackedPoint(x.toInt + 0.5, y.toInt + 0.5)

  def inRectangle(topLeft: PackedPoint, bottomRight: PackedPoint): Boolean = {
    topLeft.x <= x && topLeft.y <= y && bottomRight.x >= x && bottomRight.y >= y
  }
  def copy(x: Float = this.x, y: Float = this.y): PackedPoint = PackedPoint(x, y)
}

object PackedPoint {
  def apply(x: Float, y: Float): PackedPoint = {
    val xInt = floatToIntBits(x)
    val yInt = floatToIntBits(y)
    // Be careful to not carry over the sign bit
    PackedPoint((xInt & 0xffffffffL) | (yInt.toLong << 32))
  }

  def apply(x: Double, y: Double): PackedPoint =
    apply(x.toFloat, y.toFloat)

  val zero = PackedPoint(0, 0)

  def polar(radius: Double, angle: Double): PackedPoint = {
    val positiveRadius = Math.abs(radius)
    PackedPoint(
      positiveRadius * Math.cos(angle),
      positiveRadius * Math.sin(angle)
    )
  }

  def pointsOnArc(n: Int, start: PackedPoint, angle: Double): IndexedSeq[PackedPoint] = {
    (0.0 until n * angle by angle).map(start.rotate(_))
  }

  def regularPolygon(edges: Int, radius: Double = 1): IndexedSeq[PackedPoint] = {
    val start = PackedPoint(0, radius)
    pointsOnArc(edges, start, 2 * Math.PI / edges)
  }

  def distanceFromRectangle(p: PackedPoint, topLeft: PackedPoint, bottomRight: PackedPoint): Double = {
    Math.sqrt(
      Math.pow(List(0, topLeft.x - p.x, p.x - bottomRight.x).max, 2) +
        Math.pow(List(0, topLeft.y - p.y, p.y - bottomRight.y).max, 2)
    )
  }

  def sequence(n: Int, from: PackedPoint, to: PackedPoint): List[PackedPoint] = {
    val step = (to - from) / n
    (0 to n).toList.map(from + step * _)
  }

  def grid(topLeft: PackedPoint, bottomRight: PackedPoint, w: Int, h: Int): List[PackedPoint] = {
    val xStep = (bottomRight.x - topLeft.x) / w
    val yStep = (bottomRight.y - topLeft.y) / h
    val points = for {
      i <- 0 to w
      x = topLeft.x + i * xStep
      j <- 0 to h
      y = topLeft.y + j * yStep
    } yield PackedPoint(x, y)
    points.toList
  }

  implicit val pointGroup: Group[PackedPoint] = new Group[PackedPoint] {
    override def empty: PackedPoint = zero
    override def combine(x: PackedPoint, y: PackedPoint): PackedPoint = x + y
    override def inverse(point: PackedPoint): PackedPoint = -point
  }
}
