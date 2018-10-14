package evolution.primitive.algebra.binding.generator
import evolution.generator.Generator
import evolution.primitive.algebra.{Composed, GenRepr}
import evolution.primitive.algebra.binding.BindingAlgebra
import org.scalacheck.Gen

// TODO here only var0 differs from an Applicative-lifted Binding Algebra
class BindingAlgebraGenerator[R[_], VarName](alg: BindingAlgebra[R, VarName])
    extends BindingAlgebra[GenRepr[R, ?], Generator[VarName]] {

  override def varName(name: String): Generator[VarName] =
    Generator.pure(alg.varName(name))

  override def var0[A]: GenRepr[R, A] =
    n => if (n > 0) Generator.Unknown(Gen.const(alg.var0)) else Generator.Fail()

  override def shift[A](expr: GenRepr[R, A]): GenRepr[R, A] =
    n => expr(n - 1).map(alg.shift)

  override def let[A, B](genName: Generator[VarName], genValue: GenRepr[R, A])(genExpr: GenRepr[R, B]): GenRepr[R, B] =
    n =>
      for {
        name <- genName
        value <- genValue(n)
        expr <- genExpr(n + 1)
      } yield alg.let(name, value)(expr)

  override def lambda[A, B](genName: Generator[VarName], genExpr: GenRepr[R, B]): GenRepr[R, A => B] =
    n =>
      for {
        name <- genName
        expr <- genExpr(n + 1)
      } yield alg.lambda(name, expr)

  override def app[A, B](genF: GenRepr[R, A => B], genA: GenRepr[R, A]): GenRepr[R, B] =
    n =>
      for {
        f <- genF(n)
        a <- genA(n)
      } yield alg.app(f, a)

  override def fix[A](genExpr: GenRepr[R, A => A]): GenRepr[R, A] =
    n => genExpr(n).map(alg.fix)
}
