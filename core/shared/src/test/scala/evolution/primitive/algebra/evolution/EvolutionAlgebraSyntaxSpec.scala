package evolution.primitive.algebra.evolution
import cats.kernel.Semigroup
import cats.{Defer, MonoidK}
import cats.implicits._
import evolution.geometry.Point
import evolution.primitive.algebra.{ByVarParser, TestInterpreters}
import evolution.primitive.algebra.evolution.parser.{
  EvolutionAlgebraExpressions,
  EvolutionAlgebraGrammar,
  EvolutionAlgebraSyntax,
  LazyEvolutionAlgebraExpressions
}
import evolution.primitive.parser.CommonTestParsers
import org.scalatest.{FreeSpec, Matchers}
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi
import fastparse.noApi.{Fail, P, Parser}

class EvolutionAlgebraSyntaxSpec extends FreeSpec with Matchers with CommonTestParsers with TestInterpreters {
  "An Evolution Grammar" - {
    "should parse" - {
      "an empty evolution of Doubles" in {
        val serializedExpression = "empty"
        val expectedExpression = Lift(Empty[Double]())
        parseEvolutionOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "an empty evolution of Points" in {
        val serializedExpression = "empty"
        val expectedExpression = Lift(Empty[Point]())
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "an evolution with a single Double" in {
        val serializedExpression = "cons(1, empty)"
        val expectedExpression = Lift(Cons(1.0, Empty[Double]()))
        parseEvolutionOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "an evolution with a single point" in {}
    }
  }

  def parseEvolutionOfDoubles(serializedExpression: String): Binding[ListExpr[Double]] =
    expressions.evolutionOf(syntax.doubleConstant)(Semigroup[Double])(Nil).parse(serializedExpression).get.value

  def parseEvolutionOfPoints(serializedExpression: String): Binding[ListExpr[Point]] =
    expressions.evolutionOf(syntax.pointConstant)(Semigroup[Point])(Nil).parse(serializedExpression).get.value

  type BindingParser[T] = ByVarParser[Binding, T]

  lazy val syntax = new EvolutionAlgebraSyntax(EvolutionAlgebraTestInterpreter)
  lazy val orMonoid: MonoidK[BindingParser] = new MonoidK[BindingParser] {
    override def empty[A]: BindingParser[A] =
      _ => Fail
    override def combineK[A](x: BindingParser[A], y: BindingParser[A]): BindingParser[A] =
      ctx => P(x(ctx) | y(ctx))
  }
  lazy val defer: Defer[BindingParser] = new Defer[BindingParser] {
    override def defer[A](fa: => BindingParser[A]): BindingParser[A] =
      ctx => P(fa(ctx))
  }

  def grammar(self: EvolutionAlgebraExpressions[Constant, ListExpr, BindingParser, Parser[String]]) =
    new EvolutionAlgebraGrammar[Constant, ListExpr, BindingParser, Parser[String]](self, syntax, orMonoid)

  def expressions: EvolutionAlgebraExpressions[Constant, ListExpr, BindingParser, Parser[String]] =
    grammar(new LazyEvolutionAlgebraExpressions[Constant, ListExpr, BindingParser, Parser[String]](expressions, defer))
}
