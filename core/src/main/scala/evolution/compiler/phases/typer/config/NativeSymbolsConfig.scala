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
object NativeSymbolsConfig:
  val symbols: List[NativeSymbol] = List(
    // Comparison
    NativeSymbol(
      "greaterthan",
      Qualified(List(Predicate("Comp", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: Order[Any]) => (x: Any) => (y: Any) => p.gt(x, y)
    ),
    NativeSymbol(
      "greaterthanorequal",
      Qualified(List(Predicate("Comp", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: Order[Any]) => (x: Any) => (y: Any) => p.gteqv(x, y)
    ),
    NativeSymbol(
      "lessthan",
      Qualified(List(Predicate("Comp", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: Order[Any]) => (x: Any) => (y: Any) => p.lt(x, y)
    ),
    NativeSymbol(
      "lessthanorequal",
      Qualified(List(Predicate("Comp", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: Order[Any]) => (x: Any) => (y: Any) => p.lteqv(x, y)
    ),
    NativeSymbol(
      "eq",
      Qualified(List(Predicate("Eq", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: Eq[Any]) => (x: Any) => (y: Any) => p.eqv(x, y)
    ),
    NativeSymbol(
      "neq",
      Qualified(List(Predicate("Eq", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: Eq[Any]) => (x: Any) => (y: Any) => p.neqv(x, y)
    ),
    // boolean ops
    NativeSymbol("not", Qualified(Scheme(Bool =>: Bool)), (x: Boolean) => !x),
    NativeSymbol("or", Qualified(Scheme(Bool =>: Bool =>: Bool)), (a: Boolean) => (b: Boolean) => a || b),
    NativeSymbol("and", Qualified(Scheme(Bool =>: Bool =>: Bool)), (a: Boolean) => (b: Boolean) => a && b),
    NativeSymbol(
      "if",
      Qualified(Scheme(Bool =>: "T" =>: "T" =>: "T", "T")),
      (p: Boolean) => (a: Any) => (b: Any) => if p then a else b
    ),
    //
    // Evolutions
    NativeSymbol("empty", Qualified(Scheme(List("T"), Var("T"))), Evolution.empty),
    NativeSymbol(
      "concat",
      Qualified(Scheme(Evo("T") =>: Evo("T") =>: Evo("T"), "T")),
      curry2(Evolution.concat)
    ),
    NativeSymbol(
      "cons",
      Qualified(Scheme("T" =>: Evo("T") =>: Evo("T"), "T")),
      (x: Any) => (t: Evolution[Any]) => Evolution.cons(x, t)
    ),
    NativeSymbol("const", Qualified(Scheme("T" =>: Evo("T"), "T")), func1(Evolution.constant[Any])),
    NativeSymbol(
      "@polar",
      Qualified(Scheme(Evo(Double) =>: Evo(Double) =>: Evo(TPoint))),
      curry2(Evolution.zipWithUncurried(Point.polar))
    ),
    NativeSymbol(
      "@point",
      Qualified(Scheme(Evo(Double) =>: Evo(Double) =>: Evo(TPoint))),
      curry2(Evolution.zipWithUncurried(Point.apply))
    ),
    NativeSymbol(
      "filter",
      Qualified(Scheme(Evo("T") =>: ("T" =>: Bool) =>: Evo("T"), "T")),
      func2(Evolution.filter[Any])
    ),
    NativeSymbol(
      "map",
      Qualified(List(), Scheme(Evo("T1") =>: ("T1" =>: "T2") =>: Evo("T2"), "T1", "T2")),
      curry2(Evolution.map)
    ),
    NativeSymbol(
      "flatmap",
      Qualified(Scheme(Evo("T1") =>: ("T1" =>: Evo("T2")) =>: Evo("T2"), "T1", "T2")),
      curry2(Evolution.flatMap)
    ),
    NativeSymbol(
      "withfirst",
      Qualified(Scheme(Evo("T1") =>: ("T1" =>: Evo("T2")) =>: Evo("T2"), "T1", "T2")),
      curry2(Evolution.withFirst1)
    ),
    NativeSymbol("grouped", Qualified(Scheme(Evo("T") =>: Integer =>: Evo(Lst("T")), "T")), curry2(Evolution.grouped)),
    NativeSymbol("ordered", Qualified(Scheme(Evo(Double) =>: Integer =>: Evo(Double))), curry2(Evolution.ordered)),
    NativeSymbol("distinct", Qualified(Scheme(Evo("T") =>: Integer =>: Evo("T"), "T")), curry2(Evolution.distinct)),
    NativeSymbol("take", Qualified(Scheme(Evo("T") =>: Integer =>: Evo("T"), "T")), curry2(Evolution.take)),
    NativeSymbol(
      "slidingmap",
      Qualified(Scheme(Evo("T1") =>: ("T1" =>: "T1" =>: "T2") =>: Evo("T2"), "T1", "T2")),
      curry2(Evolution.slidingMap)
    ),
    NativeSymbol(
      "while",
      Qualified(Scheme(Evo("T") =>: ("T" =>: Bool) =>: Evo("T"), "T")),
      curry2(Evolution.takeWhile)
    ),
    NativeSymbol(
      "until",
      Qualified(Scheme(Evo("T") =>: ("T" =>: Bool) =>: Evo("T"), "T")),
      curry2(Evolution.takeUntil)
    ),
    NativeSymbol("fromlist", Qualified(Scheme(Lst("T") =>: Evo("T"), "T")), func1(Evolution.fromIterable)),
    NativeSymbol("range", Qualified(Scheme(Double =>: Double =>: Double =>: Evo(Double))), curry3(Evolution.range)),
    NativeSymbol("iterate", Qualified(Scheme(("T" =>: "T") =>: "T" =>: Evo("T"), "T")), curry2(Evolution.iterate[Any])),
    NativeSymbol(
      "iterate2",
      Qualified(Scheme(("T" =>: "T" =>: "T") =>: "T" =>: "T" =>: Evo("T"), "T")),
      curry3(Evolution.iterate2[Any])
    ),
    NativeSymbol("parallel", Qualified(Scheme(Evo(Evo("T")) =>: Evo("T"), "T")), func1(Evolution.parallel)),
    NativeSymbol(
      "parametrizations",
      Qualified(Scheme(Evo("T") =>: Integer =>: Evo(Double =>: "T"), "T")),
      curry2(Evolution.parametrizations[Any])
    ),
    NativeSymbol(
      "connect",
      Qualified(Scheme(Evo("T") =>: ("T" =>: Evo("T")) =>: Evo("T"), "T")),
      curry2(Evolution.connect)
    ),
    NativeSymbol(
      "zipwith",
      Qualified(
        Scheme(Evo("T1") =>: Evo("T2") =>: ("T1" =>: "T2" =>: "T3") =>: Evo("T3"), "T1", "T2", "T3")
      ),
      curry3(Evolution.zipWith)
    ),
    NativeSymbol("roll", Qualified(Scheme(Evo("T" =>: "T") =>: "T" =>: Evo("T"), "T")), curry2(Evolution.roll[Any])),
    NativeSymbol(
      "roll2",
      Qualified(Scheme(Evo("T" =>: "T" =>: "T") =>: "T" =>: "T" =>: Evo("T"), "T")),
      curry3(Evolution.roll2[Any])
    ),
    NativeSymbol("flatten", Qualified(Scheme(Evo(Evo("T")) =>: Evo("T"), "T")), func1(Evolution.flatten)),
    // Integrations and derivatives
    NativeSymbol(
      "integrate",
      Qualified(List(Predicate("Add", "T", "T", "T")), Scheme("T" =>: Evo("T") =>: Evo("T"), "T")),
      (p: Function2[Any, Any, Any]) => (start: Any) => (evo: Evolution[Any]) => Evolution.integrate(start, evo, p)
    ),
    NativeSymbol(
      "solve1",
      Qualified(List(Predicate("Add", "T", "T", "T")), Scheme(Evo("T" =>: "T") =>: "T" =>: Evo("T"), "T")),
      (p: Function2[Any, Any, Any]) => (evo: Evolution[Any => Any]) => (start: Any) => Evolution.solve1(evo, start, p)
    ),
    NativeSymbol(
      "solve2",
      Qualified(
        List(Predicate("Add", "T", "T", "T")),
        Scheme(Evo("T" =>: "T" =>: "T") =>: "T" =>: "T" =>: Evo("T"), "T")
      ),
      (p: Function2[Any, Any, Any]) =>
        (evo: Evolution[Any => Any => Any]) => (x0: Any) => (v0: Any) => Evolution.solve2(evo, x0, v0, p)
    ),
    NativeSymbol(
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
    NativeSymbol(
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
    NativeSymbol("uniform", Qualified(Scheme(Double =>: Double =>: Evo(Double))), curry2(Evolution.uniform)),
    NativeSymbol(
      "uniformdiscrete",
      Qualified(Scheme(Double =>: Double =>: Double =>: Evo(Double))),
      curry3(Evolution.uniformDiscrete)
    ),
    NativeSymbol("uniformchoice", Qualified(Scheme(Lst("T") =>: Evo("T"), "T")), func1(Evolution.uniformChoice)),
    NativeSymbol(
      "uniformfrom",
      Qualified(Scheme(Integer =>: Evo("T") =>: Evo("T"), "T")),
      curry2(Evolution.uniformFrom)
    ),
    NativeSymbol("normal", Qualified(Scheme(Double =>: Double =>: Evo(Double))), curry2(Evolution.normal)),
    NativeSymbol("noises", Qualified(Scheme(Evo(TPoint =>: Double))), Evolution.noiseEvolution),
    NativeSymbol(
      "octavenoises",
      Qualified(Scheme(Evo(Integer =>: Double =>: TPoint =>: Double))),
      Evolution.octaveNoiseEvolution
    ),
    // Math
    NativeSymbol("pi", Qualified(Scheme(Double)), Math.PI),
    NativeSymbol("cos", Qualified(Scheme(Double =>: Double)), func1(Math.cos)),
    NativeSymbol("sin", Qualified(Scheme(Double =>: Double)), func1(Math.sin)),
    NativeSymbol("exp", Qualified(Scheme(Double =>: Double =>: Double)), curry2(Math.pow)),
    NativeSymbol(
      "inverse",
      Qualified(List(Predicate("Invertible", "T")), Scheme("T" =>: "T", "T")),
      (inv: Function1[Any, Any]) => (x: Any) => inv(x)
    ),
    NativeSymbol("sign", Qualified(Scheme(Double =>: Double)), func1(Math.signum)),
    NativeSymbol("floor", Qualified(Scheme(Double =>: Integer)), func1(Math.floor)),
    NativeSymbol("abs", Qualified(Scheme(Double =>: Double)), (d: Double) => Math.abs(d)),
    NativeSymbol(
      "mod",
      Qualified(Scheme(Double =>: Double =>: Double)),
      (x: Double) => (y: Double) => if x >= 0 then x % y else (x % y) + y
    ),
    NativeSymbol("todbl", Qualified(Scheme(Integer =>: Double)), (x: Int) => x.toDouble),
    NativeSymbol("div", Qualified(Scheme(Double =>: Double =>: Double)), (x: Double) => (y: Double) => x / y),
    NativeSymbol(
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
    NativeSymbol(
      "multiply",
      Qualified(List(Predicate("Mult", "A", "B", "C")), Scheme("A" =>: "B" =>: "C", "A", "B", "C")),
      (p: Function2[Any, Any, Any]) => (x: Any) => (y: Any) => p(x, y)
    ),
    NativeSymbol(
      "add",
      Qualified(List(Predicate("Add", "A", "B", "C")), Scheme("A" =>: "B" =>: "C", "A", "B", "C")),
      (p: Function2[Any, Any, Any]) => (x: Any) => (y: Any) => p(x, y)
    ),
    NativeSymbol(
      "minus",
      Qualified(List(Predicate("Add", "T", "T", "T"), Predicate("Invertible", "T")), Scheme("T" =>: "T" =>: "T", "T")),
      (add: Function2[Any, Any, Any]) => (inv: Function1[Any, Any]) => (x: Any) => (y: Any) => add(x, inv(y))
    ),
    // geometry
    NativeSymbol("norm", Qualified(Scheme(TPoint =>: Double)), (p: Point) => p.norm),
    NativeSymbol("versor", Qualified(Scheme(TPoint =>: TPoint)), (p: Point) => p.versor),
    NativeSymbol("x", Qualified(Scheme(TPoint =>: Double)), (p: Point) => p.x),
    NativeSymbol("y", Qualified(Scheme(TPoint =>: Double)), (p: Point) => p.y),
    NativeSymbol("point", Qualified(Scheme(Double =>: Double =>: TPoint)), curry2(Point.apply)),
    NativeSymbol("polar", Qualified(Scheme(Double =>: Double =>: TPoint)), curry2(Point.polar)),
    NativeSymbol(
      "inrect",
      Qualified(Scheme(TPoint =>: TPoint =>: TPoint =>: Bool)),
      (topLeft: Point) => (bottomRight: Point) => (p: Point) => p.inRectangle(topLeft, bottomRight)
    ),
    //function combinators
    NativeSymbol(
      "iteratefunc",
      Qualified(Scheme(("T" =>: "T") =>: Integer =>: "T" =>: "T", "T")),
      curry2(iterateFunc[Any])
    )
  )

  val terms: Map[String, Term] = Map(symbols.map(c => c.symbol -> Value(c.value))*)

  private def func1[A, B](f: A => B): Any = f
  private def func2[A, B, C](f: A => B => C): Any = f

  private def curry2[A, B, C](f: (A, B) => C): Any = f.curried
  private def curry3[A, B, C, D](f: (A, B, C) => D): Any = f.curried

  private def iterateFunc[T](f: T => T, n: Int): T => T =
    if n <= 0 then identity[T]
    else
      val g = iterateFunc(f, n - 1)
      t => f(g(t))
