package evolution.compiler
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import evolution.compiler.types.Type
import evolution.compiler.types.TypeT
import evolution.compiler.types.TypeClasses._
import evolution.compiler.phases.parser.FastParseParser
import evolution.compiler.phases.parser.ParserConfig
import evolution.compiler.phases.typer.config.Constant
import evolution.compiler.tree._
import evolution.compiler.tree.TreeF._
import evolution.compiler.phases.parser.PrecedenceGroup.BinaryOperator

trait TreeArbitraries {
  def genFunctionArgs: Gen[List[String]] =
    for {
      n <- Gen.choose(1, 6)
      args <- Gen.listOfN(n, genLeafExpr)
    } yield args

  def genLeafExpr: Gen[String] =
    Gen.oneOf(genVarUsage, arbitrary[Double].map(_.toString))

  // TODO move all operators here
  def genOperatorWithTree: Gen[(String, BinaryOperator)] =
    Gen.oneOf[(String, BinaryOperator)](FastParseParser.binaryOperators)

  def genVarUsage: Gen[String] =
    genIdentifier.filter(id => !Constant.values.map(_.entryName).contains(id.toLowerCase))

  def genIdentifier: Gen[String] = for {
    head <- Gen.alphaChar
    tail <- Gen.alphaNumStr
  } yield head + tail

  def genLambda: Gen[String] = for {
    id <- genIdentifier
    body <- genLeafExpr
  } yield s"$id -> $body"

  def genNumber: Gen[Tree] = arbitrary[Double].map(d => DoubleLiteral(d).embed)
  def genIntNumber: Gen[Tree] = arbitrary[Int].map(n => IntLiteral(n).embed)
  def genDoubleNotIntNumber: Gen[Tree] = arbitrary[Int].map(d => DoubleLiteral((0.1 + d)).embed)

  def genTypedNumber: Gen[TypedTree] = arbitrary[Int].map(n => IntLiteral(n).annotate(Qualified(TypeT.Integer)))

  def genTypedVar: Gen[(String, TypedTree)] = for {
    id <- genIdentifier
    tpe <- genType
  } yield (id.toLowerCase, Identifier(id).annotate(Qualified(tpe)))

  def genTypedBool: Gen[TypedTree] = Gen.oneOf(true, false).map(Bool(_).annotate(Qualified(TypeT.Bool)))

  def genType: Gen[Type] = Gen.oneOf(TypeT.Double, TypeT.Bool, TypeT.Integer, TypeT.Point)

  def genVar: Gen[Tree] =
    for {
      char <- Gen.alphaChar
    } yield Identifier(char.toString).embed

  def genWhitespace: Gen[String] =
    Gen.listOf(Gen.oneOf(ParserConfig.whitespacesChars.toList)).map(_.mkString(""))
}
