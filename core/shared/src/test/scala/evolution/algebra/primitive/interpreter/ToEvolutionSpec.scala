package evolution.algebra.primitive.interpreter
import evolution.algebra.interpreter.{RNGInterpreter, StreamInterpreter}
import evolution.algebra.materializer.RNGMaterializer
import evolution.algebra.representation.RNGRepr
import evolution.primitive.algebra.interpreter.{Ctx, ToEvolution}
import evolution.random.RNG
import org.scalatest.{FreeSpec, Matchers}

class ToEvolutionSpec extends FreeSpec with Matchers {
  "The ToEvolution interpreter" - {
    "should correctly create recursive evolutions" in {
      val interpreter = new ToEvolution[Stream](new StreamInterpreter)
      import interpreter.drawing._, interpreter.scalar._, interpreter.bind._
      val stream: Ctx[Stream[Double]] = {
        fix(lambda("x", cons(double(1), var0[Stream[Double]])))
      }
      stream(Nil).take(10).toList shouldBe List.fill(10)(1.0)
    }

    "should correctly create recursive evolutions 2" in {
      val interpreter = new ToEvolution[RNGRepr](new RNGInterpreter)
      import interpreter.drawing._, interpreter.scalar._, interpreter.bind._
      val rngRepr: Ctx[RNGRepr[Double]] = {
        fix(lambda("x", cons(double(1), var0[RNGRepr[Double]])))
      }

      val stream = rngRepr(Nil).unfold(RNG(0))
      stream.take(10).toList shouldBe List.fill(10)(1.0)
    }
  }
}
