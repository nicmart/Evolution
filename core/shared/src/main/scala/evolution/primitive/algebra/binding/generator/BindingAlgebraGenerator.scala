package evolution.primitive.algebra.binding.generator
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.binding.BindingAlgebra
import org.scalacheck.Gen

class BindingAlgebraGenerator[R[_], VarName](alg: BindingAlgebra[R, VarName], varNameGen: Gen[VarName])
    extends BindingAlgebra[Composed[Gen, R, ?], Gen[VarName]] {

  override def varName(name: String): Gen[VarName] =
    Gen.const(alg.varName(name))

  override def var0[A]: Gen[R[A]] =
    Gen.const(alg.var0)

  override def shift[A](expr: Gen[R[A]]): Gen[R[A]] =
    expr.map(alg.shift)

  override def let[A, B](genName: Gen[VarName], genValue: Gen[R[A]])(genExpr: Gen[R[B]]): Gen[R[B]] =
    for {
      name <- genName
      value <- genValue
      expr <- genExpr
    } yield alg.let(name, value)(expr)

  override def lambda[A, B](genName: Gen[VarName], genExpr: Gen[R[B]]): Gen[R[A => B]] =
    for {
      name <- genName
      expr <- genExpr
    } yield alg.lambda(name, expr)

  override def app[A, B](genF: Gen[R[A => B]], genA: Gen[R[A]]): Gen[R[B]] =
    for {
      f <- genF
      a <- genA
    } yield alg.app(f, a)

  override def fix[A](genExpr: Gen[R[A => A]]): Gen[R[A]] =
    genExpr.map(alg.fix)
}
