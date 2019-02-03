package evolution.primitive
import evolution.primitive.algebra.parser.ParserConfig
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

trait ASTArbitraries[F[_]] { self: WithAst[F] =>
  import ast._

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

  def genPredefinedFunc: Gen[PredefinedFunction] =
    Gen.oneOf(PredefinedFunction.values)

  def genNumber: Gen[Expr] = withRandomTypeVar(arbitrary[Double].map(d => Expr.Number(d.toString)))
  def genIntNumber: Gen[Expr] = withRandomTypeVar(arbitrary[Int].map(d => Expr.Number(d.toString)))
  def genNotIntNumber: Gen[Expr] = withRandomTypeVar(arbitrary[Int].map(d => Expr.Number((0.1 + d).toString)))

  def genTypedNumber: Gen[Expr.Number] = arbitrary[Double].map(d => Expr.Number(d.toString, Type.Dbl))
  def genTypedVar: Gen[Expr.Var] = for {
    id <- genIdentifier
    tpe <- genType
  } yield Expr.Var(id, tpe)

  def genType: Gen[Type] = Gen.oneOf(Type.Dbl, Type.Bool, Type.Integer, Type.Point)

  def withRandomTypeVar(gen: Gen[Expr]): Gen[Expr] =
    for {
      expr <- gen
      typeChar <- Gen.alphaChar
    } yield expr.withType(Type.Var(typeChar.toString.toUpperCase))

  def genVar: Gen[Expr.Var] =
    for {
      char <- Gen.alphaChar
      typeChar <- Gen.alphaChar
    } yield Expr.Var(char.toString, Type.Var(typeChar.toString.toUpperCase))

  def genWhitespace: Gen[String] =
    Gen.listOf(Gen.oneOf(ParserConfig.whitespacesChars.map(_.mkString("")))).map(_.mkString(""))
}
