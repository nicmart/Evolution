package evolution.algebra.drawing.intepreter

import evolution.drawing.algebra._
import evolution.drawing.algebra.interpreter.{Builder, Serializer}
import evolution.geometry.Point
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}


class SerializerSpec
  extends WordSpec
    with Matchers
    with PropertyChecks {

  import Builder.start._

  "A Serializer interpreter" should {
    "serialize a complex drawing" in {
      val drawing = integrate[Point](
        Point.zero,
        point(rnd(const(-1), const(1)), rnd(const(-1), const(1)))
      )
      drawing.run(Serializer)(Nil) shouldBe "integrate(point(0.0, 0.0), point(rnd(-1.0, 1.0), rnd(-1.0, 1.0)))"
    }

    "serialize a choose expression" in {
      val drawing = choose[Point](
        const(0.5),
        const(Point.zero),
        point(const(0), rnd(const(-1), const(1)))
      )
      drawing.run(Serializer)(Nil) shouldBe "choose(0.5, point(0.0, 0.0), point(0.0, rnd(-1.0, 1.0)))"
    }
  }
}
