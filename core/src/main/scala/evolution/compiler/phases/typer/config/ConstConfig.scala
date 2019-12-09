package evolution.compiler.phases.typer.config

import evolution.compiler.impl.evaluation._
import evolution.compiler.types.Type.{Bool, Double, Evo, Integer, Lst, Scheme, Var, Point => TPoint}
import evolution.compiler.types.Type.StringTypeOps
import evolution.compiler.types.TypeClassInstance.{AdditiveInst, ComparableInst}
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}
import evolution.geometry.Point

// TODO optimizations: materialize as soon as possible
object ConstConfig {
  val constants: List[Const] = List(
    Const(
      "greaterthan",
      Qualified(List(Predicate("Comp", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: ComparableInst[Any]) => (x: Any) => (y: Any) => MaterializeComparison(p.cmp).gt(x, y)
    ),
    Const(
      "greaterthanorequal",
      Qualified(List(Predicate("Comp", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: ComparableInst[Any]) => (x: Any) => (y: Any) => MaterializeComparison(p.cmp).gteqv(x, y)
    ),
    Const(
      "lessthan",
      Qualified(List(Predicate("Comp", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: ComparableInst[Any]) => (x: Any) => (y: Any) => MaterializeComparison(p.cmp).lt(x, y)
    ),
    Const(
      "lessthanorequal",
      Qualified(List(Predicate("Comp", "T")), Scheme("T" =>: "T" =>: Bool, "T")),
      (p: ComparableInst[Any]) => (x: Any) => (y: Any) => MaterializeComparison(p.cmp).lteqv(x, y)
    ),
    Const(
      "inrect",
      Qualified(Scheme(TPoint =>: TPoint =>: TPoint =>: Bool)),
      (topLeft: Point) => (bottomRight: Point) => (p: Point) => p.inRectangle(topLeft, bottomRight)
    ),
    Const("or", Qualified(Scheme(Bool =>: Bool =>: Bool)), (a: Boolean) => (b: Boolean) => a || b),
    Const("and", Qualified(Scheme(Bool =>: Bool =>: Bool)), (a: Boolean) => (b: Boolean) => a && b),
    Const("roll", Qualified(Scheme(List("T"), Evo("T" =>: "T") =>: "T" =>: Evo("T"))), ""),
    Const("iterate", Qualified(Scheme(("T" =>: "T") =>: "T" =>: Evo("T"), "T")), ""),
    Const("flatten", Qualified(Scheme(Evo(Evo("T")) =>: Evo("T"), "T")), ""),
    Const("solve1", Qualified(Scheme(Evo("T" =>: "T") =>: "T" =>: Evo("T"), "T")), ""),
    Const("x", Qualified(Scheme(TPoint =>: Double)), ""),
    Const("pi", Qualified(Scheme(Double)), ""),
    Const("TPoint", Qualified(Scheme(Double =>: Double =>: TPoint)), ""),
    Const("octavenoise", Qualified(Scheme(Evo(Integer =>: Double =>: TPoint =>: Double))), ""),
    Const("roll2", Qualified(Scheme(Evo("T" =>: "T" =>: "T") =>: "T" =>: "T" =>: Evo("T"), "T")), ""),
    Const("empty", Qualified(Scheme(List("T"), Var("T"))), ""),
    Const(
      "minus",
      Qualified(
        List(
          Predicate("Add", "T", "T", "T"),
          Predicate("Invertible", "T")
        ),
        Scheme("T" =>: "T" =>: "T", "T")
      ),
      ""
    ),
    Const("polar", Qualified(Scheme(Double =>: Double =>: TPoint)), ""),
    Const("mod", Qualified(Scheme(Double =>: Double =>: Double)), ""),
    Const("connect", Qualified(List(), Scheme(Evo("T") =>: ("T" =>: Evo("T")) =>: Evo("T"), "T")), ""),
    Const(
      "map",
      Qualified(List(), Scheme(Evo(Var("T1")) =>: (Var("T1") =>: Var("T2")) =>: Evo(Var("T2")), "T1", "T2")),
      ""
    ),
    Const("noise", Qualified(Scheme(Evo(TPoint =>: Double))), ""),
    Const("y", Qualified(Scheme(TPoint =>: Double)), ""),
    Const("if", Qualified(Scheme(Bool =>: "T" =>: "T" =>: "T", "T")), ""),
    Const("uniformdiscrete", Qualified(Scheme(Double =>: Double =>: Double =>: Evo(Double))), ""),
    Const(
      "withfirst",
      Qualified(List(), Scheme(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2")), "T1", "T2")),
      ""
    ),
    Const("normal", Qualified(Scheme(Double =>: Double =>: Evo(Double))), ""),
    Const("fix", Qualified(Scheme(("T" =>: "T") =>: "T", "T")), ""),
    Const("concat", Qualified(Scheme(Evo("T") =>: Evo("T") =>: Evo("T"), "T")), ""),
    Const("cons", Qualified(Scheme("T" =>: Evo("T") =>: Evo("T"), "T")), ""),
    Const("todbl", Qualified(Scheme(Integer =>: Double)), ""),
    Const("div", Qualified(Scheme(Double =>: Double =>: Double)), ""),
    Const(
      "flatmap",
      Qualified(Scheme(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2")), "T1", "T2")),
      ""
    ),
    Const("cos", Qualified(Scheme(Double =>: Double)), ""),
    Const(
      "slidingmap",
      Qualified(
        Scheme(Evo(Var("T1")) =>: (Var("T1") =>: Var("T1") =>: Var("T2")) =>: Evo(Var("T2")), "T1", "T2")
      ),
      ""
    ),
    Const("exp", Qualified(Scheme(Double =>: Double =>: Double)), ""),
    Const("inverse", Qualified(Scheme("T" =>: "T", "T")), ""),
    Const("until", Qualified(Scheme(Evo("T") =>: ("T" =>: Bool) =>: Evo("T"), "T")), ""),
    Const("smoothstep", Qualified(Scheme(Double =>: Double =>: Double =>: Double)), ""),
    Const("uniform", Qualified(Scheme(Double =>: Double =>: Evo(Double))), ""),
    Const("filter", Qualified(Scheme(Evo("T") =>: ("T" =>: Bool) =>: Evo("T"), "T")), ""),
    Const("sign", Qualified(Scheme(Double =>: Double)), ""),
    Const(
      "mapwithderivative",
      Qualified(
        List(
          Predicate("Add", "T1", "T1", "T1"),
          Predicate("Invertible", "T1")
        ),
        Scheme(Evo(Var("T1")) =>: (Var("T1") =>: Var("T1") =>: Var("T2")) =>: Evo(Var("T2")), "T1", "T2")
      ),
      ""
    ),
    Const("const", Qualified(Scheme("T" =>: Evo("T"), "T")), ""),
    Const("integrate", Qualified(Scheme("T" =>: Evo("T") =>: Evo("T"), "T")), ""),
    Const("grouped", Qualified(Scheme(Evo("T") =>: Integer =>: Evo(Lst("T")), "T")), ""),
    Const("@TPoint", Qualified(Scheme(Evo(Double) =>: Evo(Double) =>: Evo(TPoint))), ""),
    Const("take", Qualified(Scheme(Evo("T") =>: Integer =>: Evo("T"), "T")), ""),
    Const("norm", Qualified(Scheme(TPoint =>: Double)), ""),
    Const("floor", Qualified(Scheme(Double =>: Integer)), ""),
    Const("range", Qualified(Scheme(Double =>: Double =>: Double =>: Evo(Double))), ""),
    Const("while", Qualified(Scheme(Evo("T") =>: ("T" =>: Bool) =>: Evo("T"), "T")), ""),
    Const("not", Qualified(Scheme(Bool =>: Bool)), ""),
    Const("iterate2", Qualified(Scheme(("T" =>: "T" =>: "T") =>: "T" =>: "T" =>: Evo("T"), "T")), ""),
    Const(
      "multiply",
      Qualified(List(Predicate("Mult", "A", "B", "C")), Scheme(Var("A") =>: Var("B") =>: Var("C"), "A", "B", "C")),
      ""
    ),
    Const("fromlist", Qualified(Scheme(Lst("T") =>: Evo("T"), "T")), ""),
    Const("abs", Qualified(Scheme(Double =>: Double)), ""),
    Const("solve2", Qualified(Scheme(Evo("T" =>: "T" =>: "T") =>: "T" =>: "T" =>: Evo("T"), "T")), ""),
    Const("uniformchoice", Qualified(Scheme(Lst("T") =>: Evo("T"), "T")), ""),
    Const("sin", Qualified(Scheme(Double =>: Double)), ""),
    Const("uniformfrom", Qualified(Scheme(Integer =>: Evo("T") =>: Evo("T"), "T")), ""),
    Const(
      "add",
      Qualified(List(Predicate("Add", "A", "B", "C")), Scheme(Var("A") =>: Var("B") =>: Var("C"), "A", "B", "C")),
      (p: AdditiveInst[Any, Any, Any]) => (x: Any) => (y: Any) => MaterializeAddition(p.add)(x, y)
    ),
    Const("@polar", Qualified(Scheme(Evo(Double) =>: Evo(Double) =>: Evo(TPoint))), ""),
    Const("eq", Qualified(List(Predicate("Comp", "T")), Scheme("T" =>: "T" =>: Bool, "T")), ""),
    Const("neq", Qualified(List(Predicate("Eq", "T")), Scheme("T" =>: "T" =>: Bool, "T")), ""),
    Const("parallel", Qualified(Scheme(List("T"), Evo(Evo("T")) =>: Evo("T"))), ""),
    Const("versor", Qualified(Scheme(TPoint =>: TPoint)), ""),
    Const(
      "derive",
      Qualified(
        List(
          Predicate("Add", "T", "T", "T"),
          Predicate("Invertible", "T")
        ),
        Scheme(Evo("T") =>: Evo("T"), "T")
      ),
      ""
    ),
    Const(
      "zipwith",
      Qualified(
        Scheme(
          Evo(Var("T1")) =>: Evo(Var("T2")) =>: (Var("T1") =>: Var("T2") =>: Var("T3")) =>: Evo(Var("T3")),
          "T1",
          "T2",
          "T3"
        )
      ),
      ""
    )
  )
}
