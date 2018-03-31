package evolution.geometry

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

class PackedPointSpec
  extends WordSpec
  with Matchers
  with PropertyChecks {

  forAll { (x: Float, y: Float) =>
    val point = Point(x, y)
    point.x shouldBe x
    point.y shouldBe y
  }
}
