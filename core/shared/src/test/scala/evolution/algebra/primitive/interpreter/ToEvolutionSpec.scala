package evolution.algebra.primitive.interpreter
import evolution.algebra.interpreter.{RNGInterpreter, StreamInterpreter}
import evolution.algebra.representation.RNGRepr
import evolution.geometry.Point
import evolution.primitive.algebra.interpreter.{Ctx, EvaluationResult, ToEvolution}
import evolution.random.RNG
import org.scalatest.{FreeSpec, Matchers}
import cats.instances.double._

class ToEvolutionSpec extends FreeSpec with Matchers {
  "The ToEvolution interpreter" - {
    "should correctly create recursive evolutions" in {
      val interpreter = new ToEvolution[Stream](new StreamInterpreter)
      import interpreter.drawing._, interpreter.scalar._, interpreter.bind._
      val stream: EvaluationResult[Stream[Double]] = {
        fix(lambda("x", cons(double(1), var0[Stream[Double]])))
      }
      stream.get(Nil).take(10).toList shouldBe List.fill(10)(1.0)
    }
//
//    "should correctly create recursive evolutions 2" ignore {
//      val interpreter = new ToEvolution[RNGRepr](new RNGInterpreter)
//      import interpreter.drawing._, interpreter.scalar._, interpreter.bind._
//      val rngRepr: Ctx[RNGRepr[Double]] = {
//        fix(lambda("x", cons(double(1), var0[RNGRepr[Double]])))
//      }
//
//      val stream = rngRepr(Nil).unfold(RNG(0))
//      stream.take(10).toList shouldBe List.fill(10)(1.0)
//    }
//
//    "should correctly create recursive evolutions 3" ignore {
//      val interpreter = new ToEvolution[Stream](new StreamInterpreter)
//      import interpreter.drawing._, interpreter.scalar._, interpreter.bind._
//      val stream: Ctx[Stream[Double]] = {
//        fix(lambda("x", cons(double(1), var0[Stream[Double]])))
//      }
//      stream(Nil).take(10).toList shouldBe List.fill(10)(1.0)
//    }
//
//    "should correctly create recursive evolutions 4" ignore {
//      val interpreter = new ToEvolution[Stream](new StreamInterpreter)
//      import interpreter.drawing._, interpreter.scalar._, interpreter.bind._
//      val stream: Ctx[Stream[Point]] = {
//        app(
//          fix(
//            lambda(
//              "self",
//              lambda("p", cons(var0[Point], app(shift(var0[Stream[Point]]), add(var0[Point], point(1, 1)))))
//            )
//          ),
//          point(0, 0)
//        )
//      }
//      stream(Nil).take(3).toList shouldBe List(Point(0, 0), Point(1, 1), Point(2, 2))
//    }
//
//    "should allow open terms to be assigned to variables" ignore {
//      val interpreter = new ToEvolution[Stream](new StreamInterpreter)
//      import interpreter.drawing._, interpreter.scalar._, interpreter.bind._, interpreter.drawing.{empty => nil}
//      val stream: Ctx[Stream[Double]] = {
//        app(lambda("x", cons(var0[Double], nil[Double])), double(1))
//      }
//
//      stream(Nil).toList shouldBe List(1.0)
//    }
//
//    "should correctly create recursive evolutions 5" ignore {
//      val interpreter = new ToEvolution[Stream](new StreamInterpreter)
//      import interpreter.drawing._, interpreter.scalar._, interpreter.bind._,
//      evolution.primitive.algebra.interpreter.BindingAlgebraDebugEvaluator.debug
//      val expr: Ctx[Stream[Double]] = {
//        def fromF: interpreter.CtxF[Double] =
//          cons(
//            debug("consHead", shift(var0[Double])),
//            debug(
//              "consTail",
//              app(
//                debug(
//                  "app func",
//                  app[Stream[Double], Double](
//                    debug("recursive f", shift(shift(var0[Stream[Double]]))),
//                    debug("addition result", var0[Double])
//                  )
//                ),
//                add(shift(var0[Double]), double(1))
//              )
//            )
//          )
//        debug("root", app(fix(debug("fromF", fromF)), double(0)))
//        //fromF
//      }
//      //def stream(ctx: List[Unit => Any]) = expr(List(() => 0.0, () => ))
//      expr(Nil).take(3) shouldBe List(1.0)
//    }
//  }
//
//  "should correctly parse open terms" ignore {
//    val interpreter = new ToEvolution[Stream](new StreamInterpreter)
//    import interpreter.drawing._, interpreter.scalar._, interpreter.bind._,
//    evolution.primitive.algebra.interpreter.BindingAlgebraDebugEvaluator.debug
//    val expr: Ctx[Double] =
//      let("f", var0[Double])(app(shift(debug("f var", var0[Double])), double(2)))
//
//    expr(Nil) shouldBe 2
//  }
  }
}
