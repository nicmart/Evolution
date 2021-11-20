package evolution.compiler.term

import evolution.compiler.LanguageSpec
import evolution.compiler.phases.typer.config.TypeclassConfig
import evolution.compiler.term.Term.Id
import evolution.compiler.types.TypeClasses.Predicate
import evolution.compiler.types.{Type, TypeClassInstance}
import evolution.geometry.Point
import evolution.materialization.Evolution
import Ordering.Double.TotalOrdering
class ConstantsInterpreterTest extends LanguageSpec:
  "comparisons" - {
    "greaterthan" in {
      val const = interpret(Id("greaterthan")).asFunc3

      const(instance("Comp", Type.Double))(1)(0) `shouldBe` true
      const(instance("Comp", Type.Double))(1)(1) `shouldBe` false
      const(instance("Comp", Type.Double))(0)(1) `shouldBe` false
    }

    "greaterthanorequal" in {
      val const = interpret(Id("greaterthanorequal")).asFunc3

      const(instance("Comp", Type.Double))(1)(0) `shouldBe` true
      const(instance("Comp", Type.Double))(1)(1) `shouldBe` true
      const(instance("Comp", Type.Double))(0)(1) `shouldBe` false
    }

    "lessthan" in {
      val const = interpret(Id("lessthan")).asFunc3

      const(instance("Comp", Type.Double))(1)(0) `shouldBe` false
      const(instance("Comp", Type.Double))(1)(1) `shouldBe` false
      const(instance("Comp", Type.Double))(0)(1) `shouldBe` true
    }

    "lessthanorequal" in {
      val const = interpret(Id("lessthanorequal")).asFunc3

      const(instance("Comp", Type.Double))(1)(0) `shouldBe` false
      const(instance("Comp", Type.Double))(1)(1) `shouldBe` true
      const(instance("Comp", Type.Double))(0)(1) `shouldBe` true
    }

    "equality" in {
      val const = interpret(Id("eq")).asFunc3
      const(instance("Eq", Type.Double))(1.1)(1.1) `shouldBe` true
      const(instance("Eq", Type.Double))(1.1)(1.2) `shouldBe` false
    }

    "non-equality" in {
      val const = interpret(Id("neq")).asFunc3
      const(instance("Eq", Type.Double))(1.1)(1.1) `shouldBe` false
      const(instance("Eq", Type.Double))(1.1)(1.2) `shouldBe` true
    }
  }

  "boolean operators" - {
    "or" in {
      val const = interpret(Id("or")).asFunc2

      const(true)(false) `shouldBe` true
      const(false)(true) `shouldBe` true
      const(true)(true) `shouldBe` true
      const(false)(false) `shouldBe` false
    }

    "and" in {
      val const = interpret(Id("and")).asFunc2

      const(true)(false) `shouldBe` false
      const(false)(true) `shouldBe` false
      const(true)(true) `shouldBe` true
      const(false)(false) `shouldBe` false
    }

    "not" in {
      val const = interpret(Id("not")).asFunc1

      const(true) `shouldBe` false
      const(false) `shouldBe` true
    }

    "ifs" in {
      val const = interpret(Id("if")).asFunc3

      const(true)("a")("b") `shouldBe` "a"
      const(false)("a")("b") `shouldBe` "b"
    }
  }

  "evolutions" - {
    "empty" in {
      val const = interpret(Id("empty"))

      const `shouldBe` Evolution.empty
    }

    "concat" in {
      val const = interpret(Id("concat")).asFunc2

      val concat = const(Evolution("a"))(Evolution("b")).asEvo
      concat.run.toList `shouldBe` List("a", "b")
    }

    "cons" in {
      val const = interpret(Id("cons")).asFunc2
      val evo = const(1)(Evolution(2, 3)).asEvo
      evo.run.toList `shouldBe` List(1, 2, 3)
    }

    "const" in {
      val const = interpret(Id("const")).asFunc1
      val evo = const(1).asEvo
      evo.run.take(10).toList `shouldBe` List.fill(10)(1)
    }

    "@polar" in {
      val const = interpret(Id("@polar")).asFunc2
      val evo = const(Evolution.constant(1))(Evolution.constant(2)).asEvo
      evo.run.take(10).toList `shouldBe` List.fill(10)(Point.polar(1, 2))
    }

    "@point" in {
      val const = interpret(Id("@point")).asFunc2
      val evo = const(Evolution.constant(1))(Evolution.constant(2)).asEvo
      evo.run.take(10).toList `shouldBe` List.fill(10)(Point(1, 2))
    }

    "filter" in {
      val const = interpret(Id("filter")).asFunc2
      val evo = const(Evolution(1, 2, 1))((n: Int) => n < 2).asEvo
      evo.run.toList `shouldBe` List(1, 1)
    }

    "map" in {
      val const = interpret(Id("map")).asFunc2
      val evo = const(Evolution(1, 2, 3))((n: Int) => n + 1).asEvo
      evo.run.toList `shouldBe` List(2, 3, 4)
    }

    "flatMap" in {
      val const = interpret(Id("flatmap")).asFunc2
      val evo = const(Evolution(1, 2, 3))((n: Int) => Evolution(n, n)).asEvo
      evo.run.toList `shouldBe` List(1, 1, 2, 2, 3, 3)
    }

    "withFirst" in {
      val const = interpret(Id("withfirst")).asFunc2
      val evo = const(Evolution(1, 2, 3))((n: Int) => Evolution(n, n)).asEvo
      evo.run.toList `shouldBe` List(1, 1)
    }

    "grouped" in {
      val const = interpret(Id("grouped")).asFunc2
      val evo = const(Evolution(1, 2, 3, 4))(2).asEvo
      evo.run.toList `shouldBe` List(List(1, 2), List(3, 4))
    }

    "take" in {
      val const = interpret(Id("take")).asFunc2
      val evo = const(Evolution(1, 2, 3, 4, 5))(2).asEvo
      evo.run.toList `shouldBe` List(1, 2)
    }

    "sliding map" in {
      val const = interpret(Id("slidingmap")).asFunc2
      val evo = const(Evolution("a", "b", "c", "d"))((a: String) => (b: String) => a + b).asEvo
      evo.run.toList `shouldBe` List("ab", "bc", "cd")
    }

    "while" in {
      val const = interpret(Id("while")).asFunc2
      val evo = const(Evolution(1, 2, 3, 4, 5))((a: Int) => a < 3).asEvo
      evo.run.toList `shouldBe` List(1, 2)
    }

    "until" in {
      val const = interpret(Id("until")).asFunc2
      val evo = const(Evolution(1, 2, 3, 4, 5))((a: Int) => a > 2).asEvo
      evo.run.toList `shouldBe` List(1, 2)
    }

    "fromlist" in {
      val const = interpret(Id("fromlist")).asFunc1
      val evo = const(List(1, 2, 3, 4, 5)).asEvo
      evo.run.toList `shouldBe` List(1, 2, 3, 4, 5)
    }

    "range" in {
      val const = interpret(Id("range")).asFunc3
      val evo = const(1)(3)(.5).asEvo
      evo.run.toList `shouldBe` List(1, 1.5, 2, 2.5, 3)
    }

    "iterate" in {
      val const = interpret(Id("iterate")).asFunc2
      val evo = const((n: Int) => n + 1)(0).asEvo
      evo.run.take(4).toList `shouldBe` List(0, 1, 2, 3)
    }

    "iterateFunc" in {
      val const = interpret(Id("iteratefunc")).asFunc3
      const((n: Int) => n + 1)(3)(0) `shouldBe` 3
    }

    "iterate2" in {
      val const = interpret(Id("iterate2")).asFunc3
      val evo = const((n: Int) => (m: Int) => n + m)(0)(1).asEvo
      evo.run.take(6).toList `shouldBe` List(0, 1, 1, 2, 3, 5)
    }

    "parallel" in {
      val const = interpret(Id("parallel")).asFunc1
      val evo = const(Evolution(Evolution(1, 2), Evolution(3, 4))).asEvo
      evo.run.toList `shouldBe` List(1, 3, 2, 4)
    }

    "parametrizations" in {
      val const = interpret(Id("parametrizations")).asFunc3
      val evoOfF = const(Evolution(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))(20).asEvo
      val f = evoOfF.run.toList.head.asInstanceOf[Double => Double]
      f(0) `shouldBe` 0
      f(1) `shouldBe` 1
      f(9) `shouldBe` 9
      f(10) `shouldBe` 9
      f(11) `shouldBe` 8
      f(-1) `shouldBe` 0
    }

    "connect" in {
      val const = interpret(Id("connect")).asFunc2
      val evo = const(Evolution(1, 2))((n: Int) => Evolution(n + 1, n + 2)).asEvo
      evo.run.toList `shouldBe` List(1, 2, 3, 4)
    }

    "zipWith" in {
      val const = interpret(Id("zipwith")).asFunc3
      val evo = const(Evolution(1, 2))(Evolution(3, 4, 5))((x: Any) => (y: Any) => (x, y)).asEvo
      evo.run.toList `shouldBe` List((1, 3), (2, 4))
    }

    "roll" in {
      val const = interpret(Id("roll")).asFunc3
      val evo = const(Evolution((x: Int) => x + 1, (x: Int) => x + 2))(0).asEvo
      evo.run.toList `shouldBe` List(0, 1, 3)
    }

    "roll2" in {
      val const = interpret(Id("roll2")).asFunc3
      val evo = const(Evolution(sum2(1), sum2(2)))(0)(1).asEvo
      evo.run.toList `shouldBe` List(0, 1, 2, 5)
    }

    "flatten" in {
      val const = interpret(Id("flatten")).asFunc1
      val evo = const(Evolution(Evolution(1, 2, 3), Evolution(), Evolution(4, 5))).asEvo
      evo.run.toList `shouldBe` List(1, 2, 3, 4, 5)
    }

    "ordered" in {
      val const = interpret(Id("ordered")).asFunc2
      val evo = Evolution(3.0, 1, 2, 4, 5)
      val ordered = const(evo)(3).asInstanceOf[Evolution[Double]]
      ordered.run.toList `shouldBe` List(1.0, 2, 3)
    }

    "distinct" in {
      val const = interpret(Id("distinct")).asFunc2
      val evo = Evolution(1, 2, 2, 3, 1, 2)
      val distinct = const(evo)(6).asInstanceOf[Evolution[Int]]
      distinct.run.toList `shouldBe` List(1, 2, 2, 3, 1, 2).distinct
    }
  }

  "integrations and derivatives" - {
    "integrate" in {
      val const = interpret(Id("integrate")).asFunc3
      val evo = const(addIntInstance)(0)(Evolution(1, 1, 1, 1)).asEvo
      evo.run.toList `shouldBe` List(0, 1, 2, 3, 4)
    }

    "solve1" in {
      val const = interpret(Id("solve1")).asFunc3
      val evo = const(addIntInstance)(Evolution(sum1(1), sum1(2)))(0).asEvo
      evo.run.toList `shouldBe` List(0, 1, 4)
    }

    "solve2" in {
      val const = interpret(Id("solve2")).asFunc4
      val evo = const(addIntInstance)(Evolution(sum2(1), sum2(2)))(0)(0).asEvo
      evo.run.toList `shouldBe` List(0, 1, 6)
    }

    "derive" in {
      val const = interpret(Id("derive")).asFunc3
      val evo = const(addIntInstance)(invIntInstance)(Evolution(1, 2, 3, 4)).asEvo
      evo.run.toList `shouldBe` List(1, 1, 1)
    }

    "mapWithDerivative" in {
      val const = interpret(Id("mapwithderivative")).asFunc4
      val evo = const(addIntInstance)(invIntInstance)(Evolution(1, 2, 3, 4))((x: Int) => (v: Int) => (x, v)).asEvo
      evo.run.toList `shouldBe` List((1, 1), (2, 1), (3, 1))
    }
  }

  "randomic evolutions" - {
    "uniform" in {
      val const = interpret(Id("uniform")).asFunc2
      val evo = const(0)(10).asEvo
      val results = evo.run.take(100).toList.asInstanceOf[List[Double]]
      results.max `should` be <= (100.0)
      results.min `should` be >= (0.0)
    }

    "uniformdiscrete" in {
      val const = interpret(Id("uniformdiscrete")).asFunc3
      val evo = const(0)(10)(5).asEvo
      val results = evo.run.take(1000).toSet
      results `shouldBe` Set(0, 5, 10)
    }

    "uniformchoice" in {
      val const = interpret(Id("uniformchoice")).asFunc1
      val evo = const(List(1, 2, 3, 4, 5)).asEvo
      val results = evo.run.take(1000).toSet
      results `shouldBe` Set(1, 2, 3, 4, 5)
    }

    "uniformFrom" in {
      val const = interpret(Id("uniformfrom")).asFunc2
      val evo = const(3)(Evolution(1, 2, 3, 4, 5)).asEvo
      val results = evo.run.take(1000).toSet
      results `shouldBe` Set(1, 2, 3)
    }

    "normal" in {
      val const = interpret(Id("normal")).asFunc2
      val evo = const(0)(1).asEvo
      val results = evo.run.take(10000).toList.asInstanceOf[List[Double]]
      Math.abs(results.sum / results.size) `should` be < .1
      results.count(x => Math.abs(x) < 3).toDouble `should` be > (.99 * 10000)
    }

    "noise" in {
      val const = interpret(Id("noises"))
      val fs = const.asEvo.run.take(100).toVector.asInstanceOf[Vector[Point => Double]]
      fs(0)(Point(0, 0)) `shouldBe` a[Double]
      fs(1)(Point(100, 50)) `shouldBe` a[Double]
    }

    "octavenoise" in {
      val const = interpret(Id("octavenoises"))
      val fs = const.asEvo.run.take(100).toVector.asInstanceOf[Vector[Int => Double => Point => Double]]
      fs(0)(1)(2)(Point(0, 0)) `shouldBe` a[Double]
      fs(1)(1)(2)(Point(100, 50)) `shouldBe` a[Double]
    }
  }

  "math" - {
    "pi" in {
      val const = interpret(Id("pi"))
      const `shouldBe` Math.PI
    }

    "cos" in {
      val const = interpret(Id("cos")).asFunc1
      const(0) `shouldBe` 1
      Math.abs(const(Math.PI / 2).asInstanceOf[Double]) `should` be < (.001)
    }

    "sin" in {
      val const = interpret(Id("sin")).asFunc1
      const(0) `shouldBe` 0
      const(Math.PI / 2) `shouldBe` 1
    }

    "exp" in {
      val const = interpret(Id("exp")).asFunc2
      const(2)(0) `shouldBe` 1
      const(2)(1) `shouldBe` 2
      const(2)(3) `shouldBe` 8
    }

    "inverse" in {
      val const = interpret(Id("inverse")).asFunc2
      const(invIntInstance)(1) `shouldBe` -1
    }

    "sign" in {
      val const = interpret(Id("sign")).asFunc1
      const(2) `shouldBe` 1
      const(-2) `shouldBe` -1
      const(0) `shouldBe` 0
    }

    "floor" in {
      val const = interpret(Id("floor")).asFunc1
      const(1.1) `shouldBe` 1
      const(-.5) `shouldBe` -1
    }

    "abs" in {
      val const = interpret(Id("abs")).asFunc1
      const(1) `shouldBe` 1
      const(-123) `shouldBe` 123
    }

    "mod" in {
      val const = interpret(Id("mod")).asFunc2
      const(7)(5) `shouldBe` 2
    }

    "toDbl" in {
      val const = interpret(Id("todbl")).asFunc1
      const(1) `shouldBe` 1.0
    }

    "div" in {
      val const = interpret(Id("div")).asFunc2
      const(3)(2) `shouldBe` 3.0 / 2
    }

    "smoothstep" in {
      val const = interpret(Id("smoothstep")).asFunc3
      const(0)(10)(-1) `shouldBe` 0
      const(0)(10)(11) `shouldBe` 1
      const(0)(10)(5) `shouldBe` .5
    }

    "add" in {
      val add = interpret(Id("add")).asFunc3

      val doubleAdd = add(instance("Add", Type.Double, Type.Double, Type.Double))
      val pointAdd = add(instance("Add", Type.Point, Type.Point, Type.Point))

      doubleAdd(1)(2) `shouldBe` 3
      pointAdd(Point(1, 2))(Point(2, 3)) `shouldBe` Point(3, 5)
    }

    "multiply" in {
      val const = interpret(Id("multiply")).asFunc3

      val doubleMult = const(instance("Mult", Type.Double, Type.Double, Type.Double))

      doubleMult(2)(3) `shouldBe` 6
    }

    "minus" in {
      val const = interpret(Id("minus")).asFunc4
      const(addIntInstance)(invIntInstance)(7)(4) `shouldBe` 3
    }
  }

  "geometry" - {
    "norm" in {
      val const = interpret(Id("norm")).asFunc1
      const(Point(0, 0)) `shouldBe` 0
      const(Point(1, 0)) `shouldBe` 1
    }

    "versor" in {
      val const = interpret(Id("versor")).asFunc1
      const(Point(10, 10)) `shouldBe` Point(10, 10).versor
    }

    "x" in {
      val const = interpret(Id("x")).asFunc1
      const(Point(10, 10)) `shouldBe` 10
    }

    "y" in {
      val const = interpret(Id("y")).asFunc1
      const(Point(10, 10)) `shouldBe` 10
    }

    "point" in {
      val const = interpret(Id("point")).asFunc2
      const(1)(2) `shouldBe` Point(1, 2)
    }

    "polar" in {
      val const = interpret(Id("polar")).asFunc2
      const(1)(2) `shouldBe` Point.polar(1, 2)
    }

    "inrect" in {
      val const = interpret(Id("inrect")).asFunc3

      const(Point(-10, -10))(Point(10, 10))(Point.zero) `shouldBe` true
      const(Point(-10, -10))(Point(-5, -5))(Point.zero) `shouldBe` false
    }
  }

  def interpret(term: Term): Any = RegisterBasedInterpreter().interpret(term)

  lazy val addIntInstance: Any = instance("Add", Type.Integer, Type.Integer, Type.Integer)
  lazy val addDoubleInstance: Any = instance("Add", Type.Integer, Type.Integer, Type.Integer)
  lazy val invIntInstance: Any = instance("Invertible", Type.Integer)

  private def instance(id: String, types: Type*): Any =
    TypeclassConfig.instance(Predicate(id, types.toList)).unsafeRight.value

  def sum1(n: Int): Int => Int = x => x + n
  def sum2(n: Int): Int => Int => Int = x => y => x + y + n

  extension (any: Any)
    def asFunc1: Any => Any = any.asInstanceOf[Any => Any]
    def asFunc2: Any => Any => Any = any.asInstanceOf[Any => Any => Any]
    def asFunc3: Any => Any => Any => Any = any.asInstanceOf[Any => Any => Any => Any]
    def asFunc4: Any => Any => Any => Any => Any = any.asInstanceOf[Any => Any => Any => Any => Any]
    def asEvo: Evolution[Any] = any.asInstanceOf[Evolution[Any]]
