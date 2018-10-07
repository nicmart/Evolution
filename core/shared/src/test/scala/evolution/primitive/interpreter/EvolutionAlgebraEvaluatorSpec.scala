package evolution.primitive.interpreter
import evolution.algebra.interpreter.StreamInterpreter
import evolution.random.RNG
import org.scalatest.{FreeSpec, Matchers}
import cats.instances.double._
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.evolution.interpreter.{EvolutionAlgebraEvaluator, EvolutionAlgebraSerializer}

class EvolutionAlgebraEvaluatorSpec extends FreeSpec with Matchers {
  "The ToEvolution interpreter" - {
    "should correctly create recursive evolutions" in {
      def drawing[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String]): R[F[Double]] = {
        import alg.list._, alg.bind._, alg.constants._
        fix(lambda("x", cons(double(1), var0[F[Double]])))
      }
      val stream = drawing(interpreter).get(Nil)
      stream.take(10).toList shouldBe List.fill(10)(1.0)
    }

    "should create an evolution of the sequence of integers" in {
      def drawing[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String]): R[F[Double]] = {
        import alg.list._, alg.bind._, alg.constants._
        app[S[Double], F[Double]](
          fix(
            lambda(
              "f",
              lambda("s", cons(var0, app(shift(var0[S[Double] => F[Double]]), add(var0[S[Double]], double(1)))))
            )
          ),
          double(0)
        )
      }
      val stream = drawing(interpreter).get(Nil)
      stream.take(10).toList shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    }

    "should be able to express integrations" in {
      def integrate[S[_], F[_], R[_], T: Semigroup](alg: EvolutionAlgebra[S, F, R, String]): R[S[T] => F[T] => F[T]] = {
        import alg.list._, alg.bind._, alg.constants._
        def varN[A](n: Int): R[A] = if (n <= 0) var0[A] else shift(varN(n - 1))

        fix[S[T] => F[T] => F[T]](
          lambda(
            "self",
            lambda(
              "start",
              lambda(
                "evolution",
                mapCons(var0[F[T]])(
                  lambda(
                    "h",
                    lambda(
                      "t",
                      cons(
                        varN[S[T]](3),
                        app(app(varN[S[T] => F[T] => F[T]](4), add(varN[S[T]](3), varN[S[T]](1))), var0[F[T]])
                      )
                    )
                  )
                )
              )
            )
          )
        )
      }

      def constant[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String]): R[F[Double]] = {
        import alg.list._, alg.bind._, alg.constants._
        fix[F[Double]](lambda("self", cons(double(1), var0[F[Double]])))
      }

      def constant2[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String]): R[F[Point]] = {
        import alg.list._, alg.bind._, alg.constants._
        fix[F[Point]](lambda("self", cons(point(double(1), double(1)), var0[F[Point]])))
      }

      def drawing[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String]): R[F[Double]] = {
        import alg.list._, alg.bind._, alg.constants._
        app(app(integrate[S, F, R, Double](alg), double(100)), constant(alg))
      }

      def drawing2[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String]): R[F[Point]] = {
        import alg.list._, alg.bind._, alg.constants._
        app(app(integrate[S, F, R, Point](alg), point(double(1), double(1))), constant2(alg))
      }

      //println(drawing2(EvolutionAlgebraSerializer)(Nil))

      val stream = drawing(interpreter).get(Nil)
      stream.take(10).toList shouldBe List(100, 101, 102, 103, 104, 105, 106, 107, 108, 109)
    }

    lazy val interpreter = new EvolutionAlgebraEvaluator[Stream](new StreamInterpreter)
  }
}
