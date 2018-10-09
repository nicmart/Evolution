package evolution.primitive.algebra.binding.generator
import evolution.primitive.algebra.{Composed, Generator}
import evolution.primitive.algebra.binding.BindingAlgebra
import org.scalacheck.Gen

class BindingAlgebraGenerator[R[_], VarName](alg: BindingAlgebra[R, VarName])
    extends BindingAlgebra[Generator[R, ?], Gen[VarName]] {

  override def varName(name: String): Gen[VarName] =
    Gen.const(alg.varName(name))

  override def var0[A]: Generator[R, A] =
    n => if (n > 0) Gen.const(alg.var0) else Gen.fail

  override def shift[A](expr: Generator[R, A]): Generator[R, A] =
    n => if (n > 1) expr(n - 1).map(alg.shift) else Gen.fail

  override def let[A, B](genName: Gen[VarName], genValue: Generator[R, A])(genExpr: Generator[R, B]): Generator[R, B] =
    n =>
      for {
        name <- genName
        value <- genValue(n)
        expr <- genExpr(n + 1)
      } yield alg.let(name, value)(expr)

  override def lambda[A, B](genName: Gen[VarName], genExpr: Generator[R, B]): Generator[R, A => B] =
    n =>
      for {
        name <- genName
        expr <- genExpr(n + 1)
      } yield alg.lambda(name, expr)

  override def app[A, B](genF: Generator[R, A => B], genA: Generator[R, A]): Generator[R, B] =
    n =>
      for {
        f <- genF(n)
        a <- genA(n)
      } yield alg.app(f, a)

  override def fix[A](genExpr: Generator[R, A => A]): Generator[R, A] =
    n => genExpr(n).map(alg.fix)
}
