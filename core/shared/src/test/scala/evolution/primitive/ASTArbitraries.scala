package evolution.primitive
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

  def genPredefinedFunc: Gen[PredefinedFunction] =
    Gen.oneOf(PredefinedFunction.values)
}
