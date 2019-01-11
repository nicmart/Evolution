package evolution.primitive.algebra.evolution.interpreter

import cats.instances.double._
import evolution.geometry.Point
import evolution.data.Evaluation
import evolution.data.EvaluationModule._
import org.scalatest.{ FreeSpec, Matchers }

class EvolutionEvaluatorSpec extends FreeSpec with Matchers {
  import interpreter.chain._, interpreter.bind._, interpreter.derived._, interpreter.constants._,
  interpreter.distribution._
  "The ToEvolution interpreter" - {
    "should correctly create recursive evolutions" in {
      val expr: R[F[Double]] = fix(lambda("x", cons(double(1), var0[F[Double]]("x"))))
      val stream = materialize(0, expr)
      stream.take(10).toList shouldBe List.fill(10)(1.0)
    }

    "should create an evolution of the sequence of integers" in {
      val expr: R[F[Double]] =
        app[Double, F[Double]](
          fix(
            lambda(
              "f",
              lambda(
                "s",
                cons(var0("s"), app(shift(var0[Double => F[Double]]("f")), add(var0[Double]("s"), double(1))))))
          ),
          double(0)
        )

      val stream = materialize(0, expr)
      stream.take(10).toList shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    }

    "should be able to map evolutions" in {
      val expr: R[F[Double]] = map(uniform(double(0), double(1)), lambda[Double, Double]("x", double(0)))

      var evalCount = Evaluation.total
      val stream = materialize(0L, expr).map(elem => {
        println("computing elem")
        println(s"Total Evaluations: ${Evaluation.total - evalCount}")
        evalCount = Evaluation.total
        elem
      })

      stream.take(4).toList shouldBe List(0, 0, 0, 0)
    }

    "should be able to express integrations" in {

      val expr: R[F[Point]] = cartesian(uniform(double(-1), double(1)), uniform(double(-1), double(1)))

      println("Desugared expression")
      //println(cartesian(new DesugarEvolutionSerializer[RNGRepr])(VectorSpace[Double])(Nil))

      println("materializing brownian stream")
      var evalCount = Evaluation.total
      val brownianStream = materialize(0L, expr).map(elem => {
        println("computing elem")
        println(s"Total Evaluations: ${Evaluation.total - evalCount}")
        evalCount = Evaluation.total
        elem
      })

      // stream.take(3).toList shouldBe List(100, 101, 102)
      // brownianStream.take(4).toList shouldBe List(100, 101, 102)

//      val stream = materialize(drawing(interpreter, double(100), double(1)))
//      stream.take(10).toList shouldBe List(100, 101, 102, 103, 104, 105, 106, 107, 108, 109)
//
//      val pointStream = materialize(drawing(interpreter, point(double(0), double(0)), point(double(1), double(1))))
//      pointStream.take(3).toList shouldBe List(Point(0, 0), Point(1, 1), Point(2, 2))
    }

    "should be to define constants" in {
      val expr: R[F[Double]] = constant(double(1))
      val stream = materialize(0, expr)
      stream.take(2).toList shouldBe List(1, 1)
    }

    "should be able combine two evolutions of doubles into one evolution of points" in {
      val expr: R[F[Point]] = cartesian(constant(double(1)), constant(double(2)))
      val stream = materialize(0, expr)
      stream.take(2).toList shouldBe List(Point(1, 2), Point(1, 2))
    }
  }
}
