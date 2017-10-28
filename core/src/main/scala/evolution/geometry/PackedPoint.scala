package evolution.geometry
import java.lang.Float._

final case class PackedPoint(packed: Long) extends AnyVal {
  def x: Float = intBitsToFloat(packed.toInt)
  def y: Float = intBitsToFloat((packed >>> 32).toInt)
}

object PackedPoint {
  def apply(x: Float, y: Float): PackedPoint = {
    val xInt = floatToIntBits(x)
    val yInt = floatToIntBits(y)
    // Be careful to not carry over the sign bit
    PackedPoint((xInt & 0xffffffffL) | (yInt.toLong << 32))
  }
}
