package evolution.primitive.algebra.evolution.interpreter

import cats.instances.double._
import cats.kernel.Semigroup
import evolution.algebra.representation.RNGRepr
import evolution.geometry.Point
import evolution.primitive.algebra.binding.interpreter.EvaluationResult
import evolution.primitive.algebra.evolution.Evolution
import evolution.random.RNG
import evolution.typeclass.VectorSpace
import org.scalatest.{ FreeSpec, Matchers }

class EvolutionEvaluatorSpec extends FreeSpec with Matchers {
  "The ToEvolution interpreter" - {
    "should correctly create recursive evolutions" in {
      def drawing[F[_], R[_]](alg: Evolution[F, R]): R[F[Double]] = {
        import alg.bind._
        import alg.chain._
        import alg.constants._
        fix(lambda("x", cons(double(1), var0[F[Double]])))
      }
      val stream = materialize(drawing(interpreter))
      stream.take(10).toList shouldBe List.fill(10)(1.0)
    }

    "should create an evolution of the sequence of integers" in {
      def drawing[F[_], R[_]](alg: Evolution[F, R]): R[F[Double]] = {
        import alg.bind._
        import alg.chain._
        import alg.constants._
        app[Double, F[Double]](
          fix(
            lambda("f", lambda("s", cons(var0, app(shift(var0[Double => F[Double]]), add(var0[Double], double(1))))))
          ),
          double(0)
        )
      }
      val stream = materialize(drawing(interpreter))
      stream.take(10).toList shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    }

    "should be able to express integrations" in {
      def drawing[F[_], R[_], T: VectorSpace](alg: Evolution[F, R], s0: R[T], v0: R[T]): R[F[T]] = {
        import alg.bind._, alg.derived._
        integrate(s0, constant(v0))
      }

      import interpreter.constants._

      val stream = materialize(drawing(interpreter, double(100), double(1)))
      stream.take(10).toList shouldBe List(100, 101, 102, 103, 104, 105, 106, 107, 108, 109)

      val pointStream = materialize(drawing(interpreter, point(double(0), double(0)), point(double(1), double(1))))
      pointStream.take(3).toList shouldBe List(Point(0, 0), Point(1, 1), Point(2, 2))
    }

    "should be to define constants" in {
      def drawing[F[_], R[_]](alg: Evolution[F, R]): R[F[Double]] = {
        import alg.bind._, alg.chain._, alg.bind._, alg.derived._, alg.constants._
        constant(double(1))
      }
      val stream = materialize(drawing(interpreter))
      stream.take(2).toList shouldBe List(1, 1)
    }

    "should be able combine two evolutions of doubles into one evolution of points" in {
      def drawing[F[_], R[_]](alg: Evolution[F, R]): R[F[Point]] = {
        import alg.bind._, alg.chain._, alg.bind._, alg.derived._, alg.constants._
        cartesian(constant(double(1)), constant(double(2)))
      }
      val stream = materialize(drawing(interpreter))
      stream.take(2).toList shouldBe List(Point(1, 2), Point(1, 2))
    }

    def materialize[T](evaluationResult: EvaluationResult[RNGRepr[T]]): Stream[T] =
      evaluationResult.get(Nil).unfold(RNG(0L))

    lazy val interpreter = EvolutionEvaluator
  }
}
