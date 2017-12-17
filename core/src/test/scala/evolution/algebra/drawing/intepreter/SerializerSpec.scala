package evolution.algebra.drawing.intepreter

import evolution.drawing.algebra.interpreter.Builder._
import evolution.drawing.algebra.interpreter.Serializer
import evolution.geometry.Point
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

class SerializerSpec
  extends WordSpec
    with Matchers
    with PropertyChecks {

  "A Serializer interpreter" should {
    "serialize a complex drawing" in {
      val drawing = integrate(
        Point.zero,
        point(rnd(-1, 1), rnd(-1, 1))
      )
      drawing.run(Serializer) shouldBe "integrate(point(0.0,0.0),point(rnd(-1.0,1.0),rnd(-1.0,1.0)))"
    }
  }
}
