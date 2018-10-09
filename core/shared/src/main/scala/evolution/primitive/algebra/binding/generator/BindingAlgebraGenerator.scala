package evolution.primitive.algebra.binding.generator
import evolution.primitive.algebra.{Composed, Generator}
import evolution.primitive.algebra.binding.BindingAlgebra
import org.scalacheck.Gen

class BindingAlgebraGenerator[R[_], VarName](alg: BindingAlgebra[R, VarName])
    extends BindingAlgebra[Generator[R, ?], Gen[VarName]] {

  override def varName(name: String): Gen[VarName] =
    Gen.const(alg.varName(name))

  override def var0[A]: Generator[R, A] =
    Gen.const(alg.var0)

  override def shift[A](expr: Generator[R, A]): Generator[R, A] =
    expr.map(alg.shift)

  override def let[A, B](genName: Gen[VarName], genValue: Generator[R, A])(genExpr: Generator[R, B]): Generator[R, B] =
    for {
      name <- genName
      value <- genValue
      expr <- genExpr
    } yield alg.let(name, value)(expr)

  override def lambda[A, B](genName: Gen[VarName], genExpr: Generator[R, B]): Generator[R, A => B] =
    for {
      name <- genName
      expr <- genExpr
    } yield alg.lambda(name, expr)

  override def app[A, B](genF: Generator[R, A => B], genA: Generator[R, A]): Generator[R, B] =
    for {
      f <- genF
      a <- genA
    } yield alg.app(f, a)

  override def fix[A](genExpr: Generator[R, A => A]): Generator[R, A] =
    genExpr.map(alg.fix)
}
