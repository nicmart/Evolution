package evolution.primitive.algebra.parser
import evolution.generator.instances.GeneratorInstances
import evolution.primitive.algebra.parser.ByVarParser.{Or, Prefixed, Pure}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Inside, Matchers}
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi._

class ByVarParserSpec
    extends FreeSpec
    with Matchers
    with Inside
    with GeneratorInstances
    with GeneratorDrivenPropertyChecks {

//  implicit override val generatorDrivenConfig =
//    PropertyCheckConfig(maxDiscarded = 100, minSuccessful = 50, maxSize = 2)

  "Or of ByVarParsers" - {

    "should flatten nested ors" in {
      forAll(genParsersWithLeaves) { parserWithLeaves =>
        parserWithLeaves.parser shouldBe Or(parserWithLeaves.leaves)
      }
    }

    "should not contain ors as children" in {
      forAll(genParsersWithLeaves) { parserWithLeaves =>
        inside(parserWithLeaves.parser) {
          case Or(children) =>
            children.foreach { isOr(_) shouldBe false }
          case _ =>
        }
      }
    }

    "should group parsers with the same prefix" in {
      forAll(genDistinctPrefixedParser) { parsersWithLeaves =>
        Or(parsersWithLeaves.map(_.parser)) shouldBe Or(parsersWithLeaves.flatMap(_.unflatten))
      }
    }
  }

  case class ParserWithLeaves(parser: ByVarParser[String], leaves: List[ByVarParser[String]])
  case class PrefixedParserWithLeaves(prefix: String, parser: ByVarParser[String], leaves: List[ByVarParser[String]]) {
    def unflatten: List[ByVarParser[String]] = leaves.map(Prefixed(prefix, _))
  }

  private def isOr[T](parser: ByVarParser[T]): Boolean = parser match {
    case Or(_) => true
    case _ => false
  }

  private def genParsersWithLeaves: Gen[ParserWithLeaves] =
    for {
      children <- Gen.listOf(genByVarRawParser)
      or <- genOrFromChildren(children)
    } yield ParserWithLeaves(or, children)

  private def genOrFromChildren(children: List[ByVarParser[String]]): Gen[ByVarParser[String]] =
    if (children.size <= 1) Gen.const(Or(children)) else genOrFromNonEmptyChildren(children)

  private def genOrFromNonEmptyChildren(children: List[ByVarParser[String]]): Gen[ByVarParser[String]] =
    for {
      splitPoint <- Gen.chooseNum(1, children.size - 1)
      (suffix, prefix) = children.splitAt(splitPoint)
      suffixParser <- genOrFromChildren(suffix)
      prefixParser <- genOrFromChildren(prefix)
    } yield Or(List(suffixParser, prefixParser))

  private def genPrefixedParser: Gen[PrefixedParserWithLeaves] =
    for {
      prefix <- withMaxSize(4, Gen.alphaNumStr)
      parsers <- Gen.listOf(genByVarRawParser)
    } yield PrefixedParserWithLeaves(prefix, Prefixed(prefix, Or(parsers)), parsers)

  private def genDistinctPrefixedParser: Gen[List[PrefixedParserWithLeaves]] =
    for {
      map <- Gen.mapOf(genPrefixedParser.map(p => (p.prefix, p)))
    } yield map.values.toList

  private def genByVarRawParser: Gen[ByVarParser[String]] = Gen.alphaNumStr.map(str => ByVarParser.Pure(str))

  private def withMaxSize[T](max: Int, gen: Gen[T]): Gen[T] =
    Gen.sized(s => Gen.resize(Math.min(s, max), gen))
}
