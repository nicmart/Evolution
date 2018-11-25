package evolution.primitive.algebra.distribution.parser

import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.parser.ByVarParser.ByVarParserK
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi._
import evolution.primitive.algebra.parser.ByVarParsers._
import fastparse.noApi.{ P, Parser }

class DistributionParserSyntax[F[_], R[_]](alg: Distribution[F, R]) extends Distribution[F, ByVarParserK[R, ?]] {
  override def uniform(
    parserFrom: ByVarParserK[R, Double],
    parserTo: ByVarParserK[R, Double]): ByVarParserK[R, F[Double]] =
    function2("uniform", parserFrom, parserTo).map {
      case (from, to) =>
        alg.uniform(from, to)
    }
}
