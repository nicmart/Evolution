package evolution.compiler.phases.typer.config

import evolution.compiler.term.Term
import evolution.compiler.term.Term.Value
import evolution.compiler.types.Type.{Bool, Double, Evo, Integer, Lst, Scheme, StringTypeOps, Var, Point as TPoint}
import evolution.compiler.types.TypeClassInstance.*
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}
import evolution.geometry.Point
import evolution.materialization.Evolution
import evolution.compiler.expression.typeclass.Multiplicative
import evolution.compiler.expression.typeclass.Invertible
import cats.kernel.Order
import cats.kernel.Eq

// TODO optimizations: materialize as soon as possible
object ConstConfig:
  val constants: List[Const] = List(
    // Comparison
    Const(
      "greaterthan",
      Qualified(List(Predicate("Comp", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: Order[Any]) => (x: Any) => (y: Any) => p.gt(x, y)
    ),
    Const(
      "greaterthanorequal",
      Qualified(List(Predicate("Comp", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: Order[Any]) => (x: Any) => (y: Any) => p.gteqv(x, y)
    ),
    Const(
      "lessthan",
      Qualified(List(Predicate("Comp", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: Order[Any]) => (x: Any) => (y: Any) => p.lt(x, y)
    ),
    Const(
      "lessthanorequal",
      Qualified(List(Predicate("Comp", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: Order[Any]) => (x: Any) => (y: Any) => p.lteqv(x, y)
    ),
    Const(
      "eq",
      Qualified(List(Predicate("Eq", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: Eq[Any]) => (x: Any) => (y: Any) => p.eqv(x, y)
    ),
    Const(
      "neq",
      Qualified(List(Predicate("Eq", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: Eq[Any]) => (x: Any) => (y: Any) => p.neqv(x, y)
    ),
    // boolean ops
    Const("not", Qualified(Scheme(Bool =>: Bool)), (x: Boolean) => !x),
    Const("or", Qualified(Scheme(Bool =>: Bool =>: Bool)), (a: Boolean) => (b: Boolean) => a || b),
    Const("and", Qualified(Scheme(Bool =>: Bool =>: Bool)), (a: Boolean) => (b: Boolean) => a && b),
    Const(
      "if",
      Qualified(Scheme(Bool =>: "T" =>: "T" =>: "T", "T")),
      (p: Boolean) => (a: Any) => (b: Any) => if p then a else b
    ),
    //
    // Evolutions
    Const("empty", Qualified(Scheme(List("T"), Var("T"))), Evolution.empty),
    Const(
      "concat",
      Qualified(Scheme(Evo("T") =>: Evo("T") =>: Evo("T"), "T")),
      curry2(Evolution.concat)
    ),
    Const(
      "cons",
      Qualified(Scheme("T" =>: Evo("T") =>: Evo("T"), "T")),
      (x: Any) => (t: Evolution[Any]) => Evolution.cons(x, t)
    ),
    Const("const", Qualified(Scheme("T" =>: Evo("T"), "T")), func1(Evolution.constant[Any])),
    Const(
      "@polar",
      Qualified(Scheme(Evo(Double) =>: Evo(Double) =>: Evo(TPoint))),
      curry2(Evolution.zipWithUncurried(Point.polar))
    ),
    Const(
      "@point",
      Qualified(Scheme(Evo(Double) =>: Evo(Double) =>: Evo(TPoint))),
      curry2(Evolution.zipWithUncurried(Point.apply))
    ),
    Const("filter", Qualified(Scheme(Evo("T") =>: ("T" =>: Bool) =>: Evo("T"), "T")), func2(Evolution.filter[Any])),
    Const(
      "map",
      Qualified(List(), Scheme(Evo("T1") =>: ("T1" =>: "T2") =>: Evo("T2"), "T1", "T2")),
      curry2(Evolution.map)
    ),
    Const(
      "flatmap",
      Qualified(Scheme(Evo("T1") =>: ("T1" =>: Evo("T2")) =>: Evo("T2"), "T1", "T2")),
      curry2(Evolution.flatMap)
    ),
    Const(
      "withfirst",
      Qualified(Scheme(Evo("T1") =>: ("T1" =>: Evo("T2")) =>: Evo("T2"), "T1", "T2")),
      curry2(Evolution.withFirst1)
    ),
    Const("grouped", Qualified(Scheme(Evo("T") =>: Integer =>: Evo(Lst("T")), "T")), curry2(Evolution.grouped)),
    Const("ordered", Qualified(Scheme(Evo(Double) =>: Integer =>: Evo(Double))), curry2(Evolution.ordered)),
    Const("distinct", Qualified(Scheme(Evo("T") =>: Integer =>: Evo("T"), "T")), curry2(Evolution.distinct)),
    Const("take", Qualified(Scheme(Evo("T") =>: Integer =>: Evo("T"), "T")), curry2(Evolution.take)),
    Const(
      "slidingmap",
      Qualified(Scheme(Evo("T1") =>: ("T1" =>: "T1" =>: "T2") =>: Evo("T2"), "T1", "T2")),
      curry2(Evolution.slidingMap)
    ),
    Const("while", Qualified(Scheme(Evo("T") =>: ("T" =>: Bool) =>: Evo("T"), "T")), curry2(Evolution.takeWhile)),
    Const("until", Qualified(Scheme(Evo("T") =>: ("T" =>: Bool) =>: Evo("T"), "T")), curry2(Evolution.takeUntil)),
    Const("fromlist", Qualified(Scheme(Lst("T") =>: Evo("T"), "T")), func1(Evolution.fromIterable)),
    Const("range", Qualified(Scheme(Double =>: Double =>: Double =>: Evo(Double))), curry3(Evolution.range)),
    Const("iterate", Qualified(Scheme(("T" =>: "T") =>: "T" =>: Evo("T"), "T")), curry2(Evolution.iterate[Any])),
    Const(
      "iterate2",
      Qualified(Scheme(("T" =>: "T" =>: "T") =>: "T" =>: "T" =>: Evo("T"), "T")),
      curry3(Evolution.iterate2[Any])
    ),
    Const("parallel", Qualified(Scheme(Evo(Evo("T")) =>: Evo("T"), "T")), func1(Evolution.parallel)),
    Const(
      "parametrizations",
      Qualified(Scheme(Evo("T") =>: Integer =>: Evo(Double =>: "T"), "T")),
      curry2(Evolution.parametrizations[Any])
    ),
    Const("connect", Qualified(Scheme(Evo("T") =>: ("T" =>: Evo("T")) =>: Evo("T"), "T")), curry2(Evolution.connect)),
    Const(
      "zipwith",
      Qualified(
        Scheme(Evo("T1") =>: Evo("T2") =>: ("T1" =>: "T2" =>: "T3") =>: Evo("T3"), "T1", "T2", "T3")
      ),
      curry3(Evolution.zipWith)
    ),
    Const("roll", Qualified(Scheme(Evo("T" =>: "T") =>: "T" =>: Evo("T"), "T")), curry2(Evolution.roll[Any])),
    Const(
      "roll2",
      Qualified(Scheme(Evo("T" =>: "T" =>: "T") =>: "T" =>: "T" =>: Evo("T"), "T")),
      curry3(Evolution.roll2[Any])
    ),
    Const("flatten", Qualified(Scheme(Evo(Evo("T")) =>: Evo("T"), "T")), func1(Evolution.flatten)),
    // Integrations and derivatives
    Const(
      "integrate",
      Qualified(List(Predicate("Add", "T", "T", "T")), Scheme("T" =>: Evo("T") =>: Evo("T"), "T")),
      (p: Function2[Any, Any, Any]) =>
        (start: Any) => (evo: Evolution[Any]) => Evolution.integrate(start, evo, p)
    ),
    Const(
      "solve1",
      Qualified(List(Predicate("Add", "T", "T", "T")), Scheme(Evo("T" =>: "T") =>: "T" =>: Evo("T"), "T")),
      (p: Function2[Any, Any, Any]) =>
        (evo: Evolution[Any => Any]) => (start: Any) => Evolution.solve1(evo, start, p)
    ),
    Const(
      "solve2",
      Qualified(
        List(Predicate("Add", "T", "T", "T")),
        Scheme(Evo("T" =>: "T" =>: "T") =>: "T" =>: "T" =>: Evo("T"), "T")
      ),
      (p: Function2[Any, Any, Any]) =>
        (evo: Evolution[Any => Any => Any]) => (x0: Any) => (v0: Any) => Evolution.solve2(evo, x0, v0, p)
    ),
    Const(
      "derive",
      Qualified(
        List(Predicate("Add", "T", "T", "T"), Predicate("Invertible", "T")),
        Scheme(Evo("T") =>: Evo("T"), "T")
      ),
      (add: Function2[Any, Any, Any]) =>
        (inv: Function1[Any, Any]) =>
          (evo: Evolution[Any]) =>
            Evolution.derive(
              evo,
              add,
              inv
            )
    ),
    Const(
      "mapwithderivative",
      Qualified(
        List(Predicate("Add", "T1", "T1", "T1"), Predicate("Invertible", "T1")),
        Scheme(Evo("T1") =>: ("T1" =>: "T1" =>: "T2") =>: Evo("T2"), "T1", "T2")
      ),
      (add: Function2[Any, Any, Any]) =>
        (inv: Function1[Any, Any]) =>
          (evo: Evolution[Any]) =>
            (f: Any => Any => Any) =>
              Evolution.mapWithDerivative(
                evo,
                f,
                add,
                inv
              )
    ),
    // Random Evolutions
    Const("uniform", Qualified(Scheme(Double =>: Double =>: Evo(Double))), curry2(Evolution.uniform)),
    Const(
      "uniformdiscrete",
      Qualified(Scheme(Double =>: Double =>: Double =>: Evo(Double))),
      curry3(Evolution.uniformDiscrete)
    ),
    Const("uniformchoice", Qualified(Scheme(Lst("T") =>: Evo("T"), "T")), func1(Evolution.uniformChoice)),
    Const("uniformfrom", Qualified(Scheme(Integer =>: Evo("T") =>: Evo("T"), "T")), curry2(Evolution.uniformFrom)),
    Const("normal", Qualified(Scheme(Double =>: Double =>: Evo(Double))), curry2(Evolution.normal)),
    Const("noises", Qualified(Scheme(Evo(TPoint =>: Double))), Evolution.noiseEvolution),
    Const(
      "octavenoises",
      Qualified(Scheme(Evo(Integer =>: Double =>: TPoint =>: Double))),
      Evolution.octaveNoiseEvolution
    ),
    // Math
    Const("pi", Qualified(Scheme(Double)), Math.PI),
    Const("cos", Qualified(Scheme(Double =>: Double)), func1(Math.cos)),
    Const("sin", Qualified(Scheme(Double =>: Double)), func1(Math.sin)),
    Const("exp", Qualified(Scheme(Double =>: Double =>: Double)), curry2(Math.pow)),
    Const(
      "inverse",
      Qualified(List(Predicate("Invertible", "T")), Scheme("T" =>: "T", "T")),
      (inv: Function1[Any, Any]) => (x: Any) => inv(x)
    ),
    Const("sign", Qualified(Scheme(Double =>: Double)), func1(Math.signum)),
    Const("floor", Qualified(Scheme(Double =>: Integer)), func1(Math.floor)),
    Const("abs", Qualified(Scheme(Double =>: Double)), (d: Double) => Math.abs(d)),
    Const(
      "mod",
      Qualified(Scheme(Double =>: Double =>: Double)),
      (x: Double) => (y: Double) => if x >= 0 then x % y else (x % y) + y
    ),
    Const("todbl", Qualified(Scheme(Integer =>: Double)), (x: Int) => x.toDouble),
    Const("div", Qualified(Scheme(Double =>: Double =>: Double)), (x: Double) => (y: Double) => x / y),
    Const(
      "smoothstep",
      Qualified(Scheme(Double =>: Double =>: Double =>: Double)),
      (from: Double) =>
        (to: Double) =>
          (position: Double) => {
            val t = (position - from) / (to - from)
            if t <= 0 then 0.0
            else if t >= 1 then 1.0
            else t * t * (3.0 - 2.0 * t)
          }
    ),
    Const(
      "multiply",
      Qualified(List(Predicate("Mult", "A", "B", "C")), Scheme("A" =>: "B" =>: "C", "A", "B", "C")),
      (p: Function2[Any, Any, Any]) => (x: Any) => (y: Any) => p(x, y)
    ),
    Const(
      "add",
      Qualified(List(Predicate("Add", "A", "B", "C")), Scheme("A" =>: "B" =>: "C", "A", "B", "C")),
      (p: Function2[Any, Any, Any]) => (x: Any) => (y: Any) => p(x, y)
    ),
    Const(
      "minus",
      Qualified(List(Predicate("Add", "T", "T", "T"), Predicate("Invertible", "T")), Scheme("T" =>: "T" =>: "T", "T")),
      (add: Function2[Any, Any, Any]) =>
        (inv: Function1[Any, Any]) => (x: Any) => (y: Any) => add(x, inv(y))
    ),
    // geometry
    Const("norm", Qualified(Scheme(TPoint =>: Double)), (p: Point) => p.norm),
    Const("versor", Qualified(Scheme(TPoint =>: TPoint)), (p: Point) => p.versor),
    Const("x", Qualified(Scheme(TPoint =>: Double)), (p: Point) => p.x),
    Const("y", Qualified(Scheme(TPoint =>: Double)), (p: Point) => p.y),
    Const("point", Qualified(Scheme(Double =>: Double =>: TPoint)), curry2(Point.apply)),
    Const("polar", Qualified(Scheme(Double =>: Double =>: TPoint)), curry2(Point.polar)),
    Const(
      "inrect",
      Qualified(Scheme(TPoint =>: TPoint =>: TPoint =>: Bool)),
      (topLeft: Point) => (bottomRight: Point) => (p: Point) => p.inRectangle(topLeft, bottomRight)
    ),
    //function combinators
    Const(
      "iteratefunc",
      Qualified(Scheme(("T" =>: "T") =>: Integer =>: "T" =>: "T", "T")),
      curry2(iterateFunc[Any])
    )
  )

  val constantsTerms: Map[String, Term] = Map(constants.map(c => c.name -> Value(c.value)) *)

  private def func1[A, B](f: A => B): Any = f
  private def func2[A, B, C](f: A => B => C): Any = f

  private def curry2[A, B, C](f: (A, B) => C): Any = f.curried
  private def curry3[A, B, C, D](f: (A, B, C) => D): Any = f.curried

  private def iterateFunc[T](f: T => T, n: Int): T => T =
    if n <= 0 then identity[T]
    else
      val g = iterateFunc(f, n - 1)
      t => f(g(t))
