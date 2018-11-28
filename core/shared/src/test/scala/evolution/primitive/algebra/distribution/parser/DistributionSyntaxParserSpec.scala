package evolution.primitive.algebra.distribution.parser
import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.evolution.interpreter.{ EvolutionTypedSerializer, Types }
import evolution.primitive.algebra.evolution.interpreter.Types.{ AnnotatedValue, F, R }
import evolution.primitive.algebra.parser.ByVarParser.{ ByVarParserK, Raw }
import evolution.primitive.algebra.parser.PrimitiveParsers
import org.scalatest.{ FreeSpec, Matchers }
import fastparse.noApi._
import fastparse.noApi.P

class DistributionSyntaxParserSpec extends FreeSpec with Matchers with PrimitiveParsers {
  val interpreter: Distribution[F, R] = (new EvolutionTypedSerializer).distribution
  val parser: Distribution[F, ByVarParserK[R, ?]] = new DistributionParserSyntax(interpreter)
  import interpreter._

  "A Distrubition Syntax should parse" - {
    "uniform" in {
      unsafeParse(s"uniform(n, n)") shouldBe "uniform(n: Double, n: Double): F[Double]"
    }
  }

  def unsafeParse[T](expression: String): String = {
    val d = R.known[Double](AnnotatedValue(Types.doubleConstant, "n"))
    val from = Raw(_ => P("n")).map(_ => d)
    val to = Raw(_ => P("n")).map(_ => d)

    parser.uniform(from, to).parser(Nil).parse(expression).get.value.infer(Types.evolutionOfDoubles).toString
  }
}
