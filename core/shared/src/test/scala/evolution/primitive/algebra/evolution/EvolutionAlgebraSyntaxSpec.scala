package evolution.primitive.algebra.evolution
import cats.kernel.Semigroup
import cats.implicits._
import evolution.geometry.Point
import evolution.primitive.algebra.{ByVarParser, TestInterpreters}
import evolution.primitive.algebra.evolution.parser.{EvolutionAlgebraExpressions, EvolutionAlgebraGrammar}
import org.scalatest.{FreeSpec, Matchers}

class EvolutionAlgebraSyntaxSpec extends FreeSpec with Matchers with TestInterpreters {
  val interpreter: EvolutionAlgebra[Constant, ListExpr, Binding, String] = EvolutionAlgebraTestInterpreter
  import interpreter.bind._, interpreter.constants._, interpreter.list._, interpreter.list.{empty => nil}

  "An Evolution Grammar" - {
    "should parse constants" - {
      "simple sum of doubles" in {
        val serializedExpression = "add(1, 2)"
        val expectedExpression =
          add(double(1), double(2))
        parseConstantOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "nested sum of doubles" in {
        val serializedExpression = "add(1, add(2, 3))"
        val expectedExpression =
          add(double(1), add(double(2), double(3)))
        parseConstantOfDoubles(serializedExpression) shouldBe expectedExpression
      }
    }

    "should parse" - {
      "an empty evolution of Doubles" in {
        val serializedExpression = "empty"
        val expectedExpression = nil[Double]
        parseEvolutionOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "an empty evolution of Points" in {
        val serializedExpression = "empty"
        val expectedExpression = nil[Point]
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "an evolution with a single Double" in {
        val serializedExpression = "cons(1, empty)"
        val expectedExpression = cons(double(1), nil)
        parseEvolutionOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "an evolution with a single point" in {
        val serializedExpression = "cons(point(0, 1), empty)"
        val expectedExpression = cons(point(double(0), double(1)), nil)
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "an evolution with two doubles" in {
        val serializedExpression = "cons(1, cons(2, empty))"
        val expectedExpression = cons(double(1), cons(double(2), nil))
        parseEvolutionOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "an evolution with two points" in {
        val serializedExpression = "cons(point(0, 0), cons(point(1, 1), empty))"
        val expectedExpression = cons(point(double(0), double(0)), cons(point(double(1), double(1)), nil))
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "an evolution where there are sums of numbers" in {
        val serializedExpression = "cons(add(1, 2), cons(add(3, add(4, 5)), empty))"
        val expectedExpression =
          cons(add(double(1), double(2)), cons(add(double(3), add(double(4), double(5))), nil))
        parseEvolutionOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "an evolution where there are sums of points" in {
        val serializedExpression = "cons(add(point(1, 1), point(3, 4)), empty)"
        val expectedExpression =
          cons(add(point(double(1), double(1)), point(double(3), double(4))), nil)
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "an evolution where there are sum of doubles inside point coordinates" in {
        val serializedExpression = "cons(point(add(1, 2), 3), empty)"
        val expectedExpression =
          cons(point(add(double(1), double(2)), double(3)), nil)
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "an evolution with a mapCons" in {
        val serializedExpression = "mapCons(empty, lambda(head)(lambda(tail)(empty)))"
        val expectedExpression =
          mapCons[Double, Double](nil)(lambda("head", lambda("tail", nil)))
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "a constant evolution defined as a fixed point" in {
        val serializedExpression = "fix(lambda(s)($s))"
        val expectedExpression: Binding[ListExpr[Double]] =
          fix(lambda("s", var0[ListExpr[Double]]))
        parseEvolutionOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "a lambda from an evolution to another" in {
        val serializedExpression = "lambda(s)($s)"
        val expectedExpression: Binding[ListExpr[Double] => ListExpr[Double]] =
          lambda("s", var0[ListExpr[Double]])
        parseLambdaOfEvolutions(serializedExpression) shouldBe expectedExpression
      }

      "an evolution expressed as a variable" in {
        val serializedExpression = "$s"
        val expectedExpression: Binding[ListExpr[Double]] =
          var0[ListExpr[Double]]
        parseEvolutionOfDoubles(serializedExpression, "s" :: Nil) shouldBe expectedExpression
      }

      "an abnormous evolution" in {
        pending
        val serializedExpression =
          "app(app(fix(lambda(self)(lambda(start)(lambda(evolution)(mapCons($evolution, lambda(h)(lambda(t)(cons($start, app(app($self, add($start, $h)), $t))))))))), point(1.0, 1.0)), fix(lambda(self)(cons(point(1.0, 1.0), $self)))"
        val expectedExpression: Binding[ListExpr[Double]] =
          var0[ListExpr[Double]]
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }
    }
  }

  def parseLambdaOfEvolutions(serializedExpression: String): Binding[ListExpr[Double] => ListExpr[Double]] =
    expressions.binding
      .function(
        expressions.list.evolutionOf(expressions.constants.doubles),
        expressions.list.evolutionOf(expressions.constants.doubles)
      )(Nil)
      .parse(serializedExpression)
      .get
      .value

  def parseEvolutionOfDoubles(
    serializedExpression: String,
    currentVars: List[String] = Nil
  ): Binding[ListExpr[Double]] =
    expressions.list
      .evolutionOf(expressions.constants.doubles)(Semigroup[Double])(currentVars)
      .parse(serializedExpression)
      .get
      .value

  def parseEvolutionOfPoints(serializedExpression: String, currentVars: List[String] = Nil): Binding[ListExpr[Point]] =
    expressions.list
      .evolutionOf(expressions.constants.points)(Semigroup[Point])(currentVars)
      .parse(serializedExpression)
      .get
      .value

  def parseConstantOfDoubles(
    serializedExpression: String,
    currentVars: List[String] = Nil
  ): Binding[Constant[Double]] = {
    expressions.constants
      .constantOf(expressions.constants.doubles)(Semigroup[Double])(currentVars)
      .parse(serializedExpression)
      .get
      .value
  }

  def parseConstantOfPoints(serializedExpression: String, currentVars: List[String] = Nil): Binding[Constant[Point]] =
    expressions.constants
      .constantOf(expressions.constants.points)(Semigroup[Point])(currentVars)
      .parse(serializedExpression)
      .get
      .value

  type BindingParser[T] = ByVarParser[Binding, T]

  def expressions: EvolutionAlgebraExpressions[Constant, ListExpr, BindingParser] =
    EvolutionAlgebraGrammar.grammar(EvolutionAlgebraTestInterpreter)
}