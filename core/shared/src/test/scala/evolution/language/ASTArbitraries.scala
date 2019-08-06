package evolution.language
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import evolution.compiler.ast.AST
import evolution.compiler.types.TypeClasses._
import evolution.compiler.types.Type
import evolution.compiler.phases.parsing.Parser
import evolution.compiler.phases.parsing.ParserConfig

trait ASTArbitraries {
  def genFunctionArgs: Gen[List[String]] =
    for {
      n <- Gen.choose(1, 6)
      args <- Gen.listOfN(n, genLeafExpr)
    } yield args

  def genLeafExpr: Gen[String] =
    Gen.oneOf(genVarUsage, arbitrary[Double].map(_.toString))

  // TODO move all operators here
  def genOperatorWithAST: Gen[(String, AST)] =
    Gen.oneOf[(String, AST)](Parser.binaryOperators)

  def genVarUsage: Gen[String] =
    genIdentifier.map(v => s"$v").filter(id => !Constant.values.map(_.entryName).contains(id.toLowerCase))

  def genIdentifier: Gen[String] = for {
    head <- Gen.alphaChar
    tail <- Gen.alphaNumStr
  } yield head + tail

  def genLambda: Gen[String] = for {
    id <- genIdentifier
    body <- genLeafExpr
  } yield s"$id -> $body"

  def genNumber: Gen[AST] = withRandomTypeVar(arbitrary[Double].map(d => AST.DoubleLiteral(d)))
  def genIntNumber: Gen[AST] = withRandomTypeVar(arbitrary[Int].map(n => AST.IntLiteral(n)))
  def genDoubleNotIntNumber: Gen[AST] = withRandomTypeVar(arbitrary[Int].map(d => AST.DoubleLiteral((0.1 + d))))

  def genTypedNumber: Gen[AST.DoubleLiteral] =
    arbitrary[Double].map(d => AST.DoubleLiteral(d, Qualified(Type.Dbl)))
  def genTypedVar: Gen[AST.Identifier] = for {
    id <- genIdentifier
    tpe <- genType
  } yield AST.Identifier(id, Qualified(tpe))

  def genBool: Gen[AST.Bool] = Gen.oneOf(true, false).map(AST.Bool(_))

  def genType: Gen[Type] = Gen.oneOf(Type.Dbl, Type.Bool, Type.Integer, Type.Point)

  def withRandomTypeVar(gen: Gen[AST]): Gen[AST] =
    for {
      expr <- gen
      typeChar <- Gen.alphaChar
    } yield expr.withType(Type.Var(typeChar.toString.toUpperCase))

  def genVar: Gen[AST.Identifier] =
    for {
      char <- Gen.alphaChar
      typeChar <- Gen.alphaChar
    } yield AST.Identifier(char.toString, Qualified(Type.Var(typeChar.toString.toUpperCase)))

  def genWhitespace: Gen[String] =
    Gen.listOf(Gen.oneOf(ParserConfig.whitespacesChars.map(_.mkString("")))).map(_.mkString(""))
}
