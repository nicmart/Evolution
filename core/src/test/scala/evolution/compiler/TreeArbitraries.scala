package evolution.compiler
import evolution.compiler.phases.parser.CatsParseParser
import evolution.compiler.phases.parser.PrecedenceGroup.BinaryOperator
import evolution.compiler.phases.typer.config.ConstConfig
import evolution.compiler.tree.TreeF._
import evolution.compiler.tree._
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

trait TreeArbitraries:
  def genFunctionArgs: Gen[List[String]] =
    for
      n <- Gen.choose(1, 6)
      args <- Gen.listOfN(n, genLeafExpr)
    yield args

  def genLeafExpr: Gen[String] =
    Gen.oneOf(genVarUsage, arbitrary[Double].map(_.toString))

  // TODO move all operators here
  def genOperatorWithTree: Gen[(String, BinaryOperator)] =
    Gen.oneOf[(String, BinaryOperator)](CatsParseParser.binaryOperators)

  def genVarUsage: Gen[String] =
    genIdentifier.filter(id => !ConstConfig.constants.map(_.name).contains(id.toLowerCase))

  def genIdentifier: Gen[String] =
    for
      head <- Gen.alphaChar
      tail <- Gen.alphaNumStr
    yield s"$head$tail"

  def genLambda: Gen[String] =
    for
      id <- genIdentifier
      body <- genLeafExpr
    yield s"$id -> $body"

  def genNumber: Gen[Tree] = arbitrary[Double].map(d => DoubleLiteral(d).embed)
  def genIntNumber: Gen[Tree] = arbitrary[Int].map(n => IntLiteral(n).embed)
  def genDoubleNotIntNumber: Gen[Tree] = arbitrary[Int].map(d => DoubleLiteral((0.1 + d)).embed)

  def genTypedNumber: Gen[TypedTree] = arbitrary[Int].map(n => IntLiteral(n).annotate(Qualified(Type.Integer)))

  def genTypedVar: Gen[(String, TypedTree)] =
    for
      id <- genIdentifier
      tpe <- genType
    yield (id.toLowerCase, Id(CaseInsensitiveName(id)).annotate(Qualified(tpe)))

  def genTypedBool: Gen[TypedTree] = Gen.oneOf(true, false).map(Bool(_).annotate(Qualified(Type.Bool)))

  def genType: Gen[Type] = Gen.oneOf(Type.Double, Type.Bool, Type.Integer, Type.Point)

  def genVar: Gen[Tree] =
    for char <- Gen.alphaChar
    yield Id(CaseInsensitiveName(char.toString)).embed

  def genWhitespace: Gen[String] =
    Gen.listOf(Gen.oneOf(List(" ", "\n", "\r", "\t"))).map(_.mkString(""))
