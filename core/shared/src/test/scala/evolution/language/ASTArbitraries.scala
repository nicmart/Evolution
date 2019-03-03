package evolution.language
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

trait ASTArbitraries[F[_]] { self: ASTModule[F] =>

  def genFunctionArgs: Gen[List[String]] =
    for {
      n <- Gen.choose(1, 6)
      args <- Gen.listOfN(n, genLeafExpr)
    } yield args

  def genLeafExpr: Gen[String] =
    Gen.oneOf(genVarUsage, arbitrary[Double].map(_.toString))

  def genVarUsage: Gen[String] = genIdentifier.map(v => s"$$$v")

  def genIdentifier: Gen[String] = for {
    head <- Gen.alphaChar
    tail <- Gen.alphaNumStr
  } yield head + tail

  def genLambda: Gen[String] = for {
    id <- genIdentifier
    body <- genLeafExpr
  } yield s"$id -> $body"

  def genPredefinedFunc: Gen[Constant] =
    Gen.oneOf(Constant.nonFunctions0)

  def genNumber: Gen[AST] = withRandomTypeVar(arbitrary[Double].map(d => AST.Number(d.toString)))
  def genIntNumber: Gen[AST] = withRandomTypeVar(arbitrary[Int].map(d => AST.Number(d.toString)))
  def genNotIntNumber: Gen[AST] = withRandomTypeVar(arbitrary[Int].map(d => AST.Number((0.1 + d).toString)))

  def genTypedNumber: Gen[AST.Number] = arbitrary[Double].map(d => AST.Number(d.toString, Type.Dbl))
  def genTypedVar: Gen[AST.Var] = for {
    id <- genIdentifier
    tpe <- genType
  } yield AST.Var(id, tpe)

  def genType: Gen[Type] = Gen.oneOf(Type.Dbl, Type.Bool, Type.Integer, Type.Point)

  def withRandomTypeVar(gen: Gen[AST]): Gen[AST] =
    for {
      expr <- gen
      typeChar <- Gen.alphaChar
    } yield expr.withType(Type.Var(typeChar.toString.toUpperCase))

  def genVar: Gen[AST.Var] =
    for {
      char <- Gen.alphaChar
      typeChar <- Gen.alphaChar
    } yield AST.Var(char.toString, Type.Var(typeChar.toString.toUpperCase))

  def genWhitespace: Gen[String] =
    Gen.listOf(Gen.oneOf(ParserConfig.whitespacesChars.map(_.mkString("")))).map(_.mkString(""))
}