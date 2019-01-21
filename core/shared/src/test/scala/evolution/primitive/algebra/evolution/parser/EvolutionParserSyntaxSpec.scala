package evolution.primitive.algebra.evolution.parser

import evolution.geometry.Point
import evolution.typeclass.VectorSpace._
import evolution.primitive.algebra.evolution.interpreter.Types._
import evolution.primitive.algebra.evolution.interpreter.{ EvolutionTypedSerializer, Types }
import evolution.primitive.algebra.parser.ByVarParser.ByVarParserK
import fastparse.noApi
import org.scalatest.{ FreeSpec, Matchers }

class EvolutionParserSyntaxSpec extends FreeSpec with Matchers {
  val interpreter = new EvolutionTypedSerializer
  import interpreter.bind._
  import interpreter.chain.{ empty => nil, _ }
  import interpreter.constants._

  "An Evolution Grammar" - {
    "should parse constants" - {
      "simple sum of doubles" in {
        val serializedExpression = "add(1, 2)"
        val expectedExpression =
          add(double(1), double(2)).infer(doubleConstant).toString
        parseConstantOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "nested sum of doubles" in {
        val serializedExpression = "add(1, add(2, 3))"
        val expectedExpression =
          add(double(1), add(double(2), double(3))).infer(doubleConstant).toString
        parseConstantOfDoubles(serializedExpression) shouldBe expectedExpression
      }
    }

    "should parse" - {
      "an empty evolution of Doubles" in {
        val serializedExpression = "empty"
        val expectedExpression = nil[Double].infer(evolutionOfDoubles).toString
        parseEvolutionOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "an empty evolution of Points" in {
        val serializedExpression = "empty"
        val expectedExpression = nil[Point].infer(evolutionOfPoints).toString
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "an evolution with a single Double" in {
        val serializedExpression = "cons(1, empty)"
        val expectedExpression = cons(double(1), nil).infer(evolutionOfDoubles).toString
        parseEvolutionOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "an evolution with a single point" in {
        val serializedExpression = "cons(point(0, 1), empty)"
        val expectedExpression = cons(point(double(0), double(1)), nil).infer(evolutionOfPoints).toString
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "an evolution with two doubles" in {
        val serializedExpression = "cons(1, cons(2, empty))"
        val expectedExpression = cons(double(1), cons(double(2), nil)).infer(evolutionOfDoubles).toString
        parseEvolutionOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "an evolution with two points" in {
        val serializedExpression = "cons(point(0, 0), cons(point(1, 1), empty))"
        val expectedExpression =
          cons(point(double(0), double(0)), cons(point(double(1), double(1)), nil)).infer(evolutionOfPoints).toString
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "an evolution where there are sums of numbers" in {
        val serializedExpression = "cons(add(1, 2), cons(add(3, add(4, 5)), empty))"
        val expectedExpression =
          cons(add(double(1), double(2)), cons(add(double(3), add(double(4), double(5))), nil))
            .infer(evolutionOfDoubles)
            .toString
        parseEvolutionOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "an evolution where there are sums of points" in {
        val serializedExpression = "cons(add(point(1, 1), point(3, 4)), empty)"
        val expectedExpression =
          cons(add(point(double(1), double(1)), point(double(3), double(4))), nil).infer(evolutionOfPoints).toString
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "an evolution where there are sum of doubles inside point coordinates" in {
        val serializedExpression = "cons(point(add(1, 2), 3), empty)"
        val expectedExpression =
          cons(point(add(double(1), double(2)), double(3)), nil).infer(evolutionOfPoints).toString
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }

      "an evolution with a mapCons" in {
        val serializedExpression = "mapCons(empty, head -> tail -> empty)"
        val expectedExpression =
          mapCons[Double, Double](nil)(lambda("head", lambda("tail", nil))).infer(evolutionOfDoubles).toString
        parseEvolutionOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "a constant evolution defined as a fixed point" in {
        pending
        val serializedExpression = "fix(s -> $s)"
        val expectedExpression: String =
          fix[F[Double]](lambda("s", var0[F[Double]]("s"))).infer(evolutionOfDoubles).toString
        parseEvolutionOfDoubles(serializedExpression) shouldBe expectedExpression
      }

      "a lambda from an evolution to another" in {
        val serializedExpression = "s -> $s"
        val expectedExpression: String =
          lambda("s", var0[F[Double]]("s")).infer(lambdaTypeInfo).toString
        parseLambdaOfEvolutions(serializedExpression) shouldBe expectedExpression
      }

      "an evolution expressed as a variable" in {
        val serializedExpression = "$s"
        val expectedExpression: String =
          var0[F[Double]]("s").infer(evolutionOfDoubles).toString
        parseEvolutionOfDoubles(serializedExpression, "s" :: Nil) shouldBe expectedExpression
      }

      /***
       * The cut makes the parse fail for this reasons:
       * 1. The expressions to be parsed is known to be a Point
       * 2. "app" is parsed and then any lambda that returns a Point is accepted (allApp in BindingSyntax)
       * 3. There are 4 available lambdas, expressed in an OR that contains Raws
       * 4. Inside the first Raw for the lambda say Double -> Point there is another OR, with
       *    a fix inside that parses successfully with a CUT
       * 5. "app(fix(f->$f)" is parsed, and then the parser fails because it does not find a double
       * 6. The parser does not try the other lambdas because of the CUT.
       * 7. The parsing fails
       * */
      "an ambiguous evolution" in {
        val serializedExpression = "app(fix(f->$f),point(0,0))"
        val expectedExpression: String =
          app[Point, Point](fix(lambda("f", var0("f"))), point(double(0), double(0))).infer(pointConstant).toString
        parseConstantOfPoints(serializedExpression) shouldBe expectedExpression
      }

      // TODO let's change the interpreter to the typed serialiser before implementing this
      "a uniform distribution of doubles" in {
        pending
      }

      // TODO write a valid expectation
      "an abnormous evolution" in {
        pending
        val serializedExpression =
          "app(app(fix(self->start->evolution->mapCons($evolution,h->t->cons($start,app(app($self,add($start,$h)),$t)))),point(1.0,1.0)),fix(self->cons(point(1.0,1.0),$self)))"
        val expectedExpression = ""
        parseEvolutionOfPoints(serializedExpression) shouldBe expectedExpression
      }
    }
  }

  lazy val lambdaTypeInfo = FunctionTypeInfo(doubleConstant, doubleConstant)

  def parseLambdaOfEvolutions(serializedExpression: String): String =
    expressions
      .function(expressions.evolutionOfDoubles, expressions.evolutionOfDoubles)
      .parser(Nil)
      .parse(serializedExpression)
      .get
      .value
      .infer(lambdaTypeInfo)
      .toString

  def parseEvolutionOfDoubles(
    serializedExpression: String,
    currentVars: List[String] = Nil
  ): String =
    expressions.evolutionOfDoubles
      .parser(currentVars)
      .parse(serializedExpression)
      .get
      .value
      .infer(evolutionOfDoubles)
      .toString

  def parseEvolutionOfPoints(serializedExpression: String, currentVars: List[String] = Nil): String =
    expressions.evolutionOfPoints
      .parser(currentVars)
      .parse(serializedExpression)
      .get
      .value
      .infer(evolutionOfPoints)
      .toString

  def parseConstantOfDoubles(serializedExpression: String, currentVars: List[String] = Nil): String = {
    expressions.doubleConstant
      .loggingParser(currentVars)
      .parse(serializedExpression)
      .get
      .value
      .infer(doubleConstant)
      .toString
  }

  def parseConstantOfPoints(serializedExpression: String, currentVars: List[String] = Nil): String =
    expressions.pointConstant
      .loggingParser(currentVars)
      .parse(serializedExpression)
      .get
      .value
      .infer(pointConstant)
      .toString

  val expressions: Expressions[F, ByVarParserK[R, ?]] =
    EvolutionGrammar.parserGrammar(interpreter)
}
