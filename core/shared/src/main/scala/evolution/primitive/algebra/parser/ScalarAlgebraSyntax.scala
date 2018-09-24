package evolution.primitive.algebra.parser

import cats.instances.double._
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.{CoreDrawingAlgebra, ScalarAlgebra}
import evolution.primitive.algebra.parser.ParsersContainerOps._
import ParserConfig.White._
import fastparse.noApi._
import PrimitiveParsers._
import cats.{Defer, MonoidK}

object ScalarAlgebra {

//double.map(alg.double)
//function2("point", double, double).map { case (x, y) => alg.point(x, y) }
  class Syntax[S[_]](alg: ScalarAlgebra[S]) extends ScalarAlgebra[λ[α => Parser[S[α]]]] {
    override def double(d: Double): Parser[S[Double]] =
      P(d.toString).map(_ => alg.double(d))
    override def point(x: Double, y: Double): Parser[S[Point]] =
      function2("point", double(x), double(y)).map(_ => alg.point(x, y))
    override def add[T: Semigroup](a: Parser[S[T]], b: Parser[S[T]]): Parser[S[T]] =
      function2("add", a, b).map { case (parsedA, parsedB) => alg.add(parsedA, parsedB) }
  }

  trait Expressions[S[_]] {
    def get[T: Semigroup](self: S[T]): S[T]
  }

  class LazyExpressions[S[_]](expressions: => Expressions[S], defer: Defer[S]) extends Expressions[S] {
    override def get[T: Semigroup](self: S[T]): S[T] = defer.defer(expressions.get(self))
  }

  class Grammar[S[_]](self: Expressions[S], syntax: ScalarAlgebra[S], orMonoid: MonoidK[S]) extends Expressions[S] {
    override def get[T: Semigroup](t: S[T]): S[T] =
      or(t, syntax.add(self.get(t), self.get(t)))

    private def or[T](expressions: S[T]*): S[T] =
      expressions.foldLeft(orMonoid.empty[T])(orMonoid.combineK[T])
  }
}
