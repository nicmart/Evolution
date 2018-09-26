package evolution.algebra.primitive.interpreter
import evolution.algebra.interpreter.{RNGInterpreter, StreamInterpreter}
import evolution.primitive.algebra.interpreter.ToEvolution
import evolution.random.RNG
import org.scalatest.{FreeSpec, Matchers}
import cats.instances.double._
import evolution.primitive.algebra.evolution.EvolutionAlgebra
class ToEvolutionSpec extends FreeSpec with Matchers {
  "The ToEvolution interpreter" - {
    "should correctly create recursive evolutions" in {
      def drawing[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String]): R[F[Double]] = {
        import alg.drawing._, alg.bind._, alg.scalar._
        fix(lambda("x", cons(double(1), var0[F[Double]])))
      }
      val stream = drawing(interpreter).get(Nil)
      stream.take(10).toList shouldBe List.fill(10)(1.0)
    }

    "should create an evolution of the sequence of integers" in {
      def drawing[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String]): R[F[Double]] = {
        import alg.drawing._, alg.bind._, alg.scalar._
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
      def integrate[S[_], F[_], R[_]](
        alg: EvolutionAlgebra[S, F, R, String]
      ): R[S[Double] => F[Double] => F[Double]] = {
        import alg.drawing._, alg.bind._, alg.scalar._
        def varN[T](n: Int): R[T] = if (n <= 0) var0[T] else shift(varN(n - 1))

        fix[S[Double] => F[Double] => F[Double]](
          lambda(
            "self",
            lambda(
              "start",
              lambda(
                "evolution",
                mapCons(var0[F[Double]])(
                  lambda(
                    "h",
                    lambda(
                      "t",
                      cons(
                        varN[S[Double]](3),
                        app(
                          app(
                            varN[S[Double] => F[Double] => F[Double]](4),
                            add(varN[S[Double]](3), varN[S[Double]](1))
                          ),
                          var0[F[Double]]
                        )
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
        import alg.drawing._, alg.bind._, alg.scalar._
        fix[F[Double]](lambda("self", cons(double(1), var0[F[Double]])))
      }

      def drawing[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String]): R[F[Double]] = {
        import alg.drawing._, alg.bind._, alg.scalar._
        app(app(integrate(alg), double(100)), constant(alg))
      }

      val stream = drawing(interpreter).get(Nil)
      stream.take(10).toList shouldBe List(100, 101, 102, 103, 104, 105, 106, 107, 108, 109)
    }

    lazy val interpreter = new ToEvolution[Stream](new StreamInterpreter)
  }
}
