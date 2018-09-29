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

      "an evolution with a single point" in {
        val serializedExpression = "cons(point(0, 1), empty)"
        val expectedExpression = Lift(Cons(PointConstant(0.0, 1.0), Empty[Point]()))
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "an evolution with two doubles" in {
        val serializedExpression = "cons(1, cons(2, empty))"
        val expectedExpression = Lift(Cons(1.0, Cons(2.0, Empty[Double]())))
        parseEvolutionOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "an evolution with two points" in {
        val serializedExpression = "cons(point(0, 0), cons(point(1, 1), empty))"
        val expectedExpression = Lift(Cons(PointConstant(0, 0), Cons(PointConstant(1, 1), Empty[Point]())))
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "an evolution where there are sums of numbers" in {
        val serializedExpression = "cons(add(1, 2), cons(add(3, add(4, 5)), empty))"
        val expectedExpression =
          Lift(Cons[Double](Add[Double](1, 2), Cons[Double](Add[Double](3, Add[Double](4, 5)), Empty[Double]())))
        parseEvolutionOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "an evolution where there are sums of points" in {
        val serializedExpression = "cons(add(point(1, 1), point(3, 4)), empty)"
        val expectedExpression =
          Lift(Cons[Point](Add[Point](PointConstant(1, 1), PointConstant(3, 4)), Empty[Point]()))
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "an evolution where there are sum of doubles inside point coordinates" in {
        val serializedExpression = "cons(point(add(1, 2), 3), empty)"
        val expectedExpression =
          Lift(Cons[Point](PointConstant(Add(1.0, 2.0), 3.0), Empty[Point]()))
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "an evolution with a mapCons" in {
        val serializedExpression = "mapCons(empty, lambda(head)(lambda(tail)(empty)))"
        val expectedExpression =
          Lift(
            MapCons(
              Empty[Double](),
              Lambda[Constant[Double], ListExpr[Double] => ListExpr[Double]](
                "head",
                Lambda[ListExpr[Double], ListExpr[Double]]("tail", Lift(Empty[Double]()))
              )
            )
          )
        pending
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }
    }
  }

  def parseEvolutionOfDoubles(serializedExpression: String): Binding[ListExpr[Double]] =
    expressions.evolutionOfDoubles(Nil).parse(serializedExpression).get.value

  def parseEvolutionOfPoints(serializedExpression: String): Binding[ListExpr[Point]] =
    expressions.evolutionOfPoints(Nil).parse(serializedExpression).get.value

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
    new EvolutionAlgebraGrammar[Constant, ListExpr, BindingParser, Parser[String]](
      self,
      syntax,
      orMonoid,
      syntax.doubleConstant
    )

  def expressions: EvolutionAlgebraExpressions[Constant, ListExpr, BindingParser, Parser[String]] =
    grammar(new LazyEvolutionAlgebraExpressions[Constant, ListExpr, BindingParser, Parser[String]](expressions, defer))
}
