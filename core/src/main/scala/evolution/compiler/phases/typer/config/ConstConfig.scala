package evolution.compiler.phases.typer.config

import evolution.compiler.impl.evaluation._
import evolution.compiler.types.Type.{Bool, Double, Evo, Integer, Lst, Scheme, Var, Point => TPoint}
import evolution.compiler.types.TypeClassInstance.{AdditiveInst, ComparableInst}
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}
import evolution.geometry.Point

// TODO optimizations: materialize as soon as possible
object ConstConfig {
  val constants: List[Const] = List(
    Const(
      "greaterthan",
      Qualified(
        List(Predicate("Comp", List(Var("T")))),
        Scheme(List("T"), Var("T") =>: Var("T") =>: Bool)
      ),
      (p: ComparableInst[Any]) => (x: Any) => (y: Any) => MaterializeComparison(p.cmp).gt(x, y)
    ),
    Const(
      "greaterthanorequal",
      Qualified(
        List(Predicate("Comp", List(Var("T")))),
        Scheme(List("T"), Var("T") =>: Var("T") =>: Bool)
      ),
      (p: ComparableInst[Any]) => (x: Any) => (y: Any) => MaterializeComparison(p.cmp).gteqv(x, y)
    ),
    Const(
      "lessthan",
      Qualified(
        List(Predicate("Comp", List(Var("T")))),
        Scheme(List("T"), Var("T") =>: Var("T") =>: Bool)
      ),
      (p: ComparableInst[Any]) => (x: Any) => (y: Any) => MaterializeComparison(p.cmp).lt(x, y)
    ),
    Const(
      "lessthanorequal",
      Qualified(
        List(Predicate("Comp", List(Var("T")))),
        Scheme(List("T"), Var("T") =>: Var("T") =>: Bool)
      ),
      (p: ComparableInst[Any]) => (x: Any) => (y: Any) => MaterializeComparison(p.cmp).lteqv(x, y)
    ),
    Const(
      "inrect",
      Qualified(Scheme(TPoint =>: TPoint =>: TPoint =>: Bool)),
      (topLeft: Point) => (bottomRight: Point) => (p: Point) => p.inRectangle(topLeft, bottomRight)
    ),
    Const("or", Qualified(Scheme(Bool =>: Bool =>: Bool)), (a: Boolean) => (b: Boolean) => a || b),
    Const("and", Qualified(Scheme(Bool =>: Bool =>: Bool)), (a: Boolean) => (b: Boolean) => a && b),
    Const(
      "roll",
      Qualified(Scheme(List("T"), Evo(Var("T") =>: Var("T")) =>: Var("T") =>: Evo(Var("T")))),
      ""
    ),
    Const(
      "iterate",
      Qualified(Scheme(List("T"), (Var("T") =>: Var("T")) =>: Var("T") =>: Evo(Var("T")))),
      ""
    ),
    Const("flatten", Qualified(Scheme(List("T"), Evo(Evo(Var("T"))) =>: Evo(Var("T")))), ""),
    Const(
      "solve1",
      Qualified(Scheme(List("T"), Evo(Var("T") =>: Var("T")) =>: Var("T") =>: Evo(Var("T")))),
      ""
    ),
    Const("x", Qualified(Scheme(TPoint =>: Double)), ""),
    Const("pi", Qualified(Scheme(Double)), ""),
    Const("TPoint", Qualified(Scheme(Double =>: Double =>: TPoint)), ""),
    Const(
      "octavenoise",
      Qualified(Scheme(Evo(Integer =>: Double =>: TPoint =>: Double))),
      ""
    ),
    Const(
      "roll2",
      Qualified(
        List(),
        Scheme(
          List("T"),
          Evo(Var("T") =>: Var("T") =>: Var("T")) =>: Var("T") =>: Var("T") =>: Evo(Var("T"))
        )
      ),
      ""
    ),
    Const("empty", Qualified(Scheme(List("T"), Var("T"))), ""),
    Const(
      "minus",
      Qualified(
        List(
          Predicate("Add", List(Var("T"), Var("T"), Var("T"))),
          Predicate("Invertible", List(Var("T")))
        ),
        Scheme(List("T"), Var("T") =>: Var("T") =>: Var("T"))
      ),
      ""
    ),
    Const("polar", Qualified(Scheme(Double =>: Double =>: TPoint)), ""),
    Const("mod", Qualified(Scheme(Double =>: Double =>: Double)), ""),
    Const(
      "connect",
      Qualified(
        List(),
        Scheme(List("T"), Evo(Var("T")) =>: (Var("T") =>: Evo(Var("T"))) =>: Evo(Var("T")))
      ),
      ""
    ),
    Const(
      "map",
      Qualified(
        List(),
        Scheme(List("T1", "T2"), Evo(Var("T1")) =>: (Var("T1") =>: Var("T2")) =>: Evo(Var("T2")))
      ),
      ""
    ),
    Const("noise", Qualified(Scheme(Evo(TPoint =>: Double))), ""),
    Const("y", Qualified(Scheme(TPoint =>: Double)), ""),
    Const("if", Qualified(Scheme(List("T"), Bool =>: Var("T") =>: Var("T") =>: Var("T"))), ""),
    Const(
      "uniformdiscrete",
      Qualified(Scheme(Double =>: Double =>: Double =>: Evo(Double))),
      ""
    ),
    Const(
      "withfirst",
      Qualified(
        List(),
        Scheme(List("T1", "T2"), Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2")))
      ),
      ""
    ),
    Const("normal", Qualified(Scheme(Double =>: Double =>: Evo(Double))), ""),
    Const("fix", Qualified(Scheme(List("T"), (Var("T") =>: Var("T")) =>: Var("T"))), ""),
    Const(
      "concat",
      Qualified(Scheme(List("T"), Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T")))),
      ""
    ),
    Const(
      "cons",
      Qualified(Scheme(List("T"), Var("T") =>: Evo(Var("T")) =>: Evo(Var("T")))),
      ""
    ),
    Const("todbl", Qualified(Scheme(Integer =>: Double)), ""),
    Const("div", Qualified(Scheme(Double =>: Double =>: Double)), ""),
    Const(
      "flatmap",
      Qualified(
        List(),
        Scheme(List("T1", "T2"), Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2")))
      ),
      ""
    ),
    Const("cos", Qualified(Scheme(Double =>: Double)), ""),
    Const(
      "slidingmap",
      Qualified(
        List(),
        Scheme(
          List("T1", "T2"),
          Evo(Var("T1")) =>: (Var("T1") =>: Var("T1") =>: Var("T2")) =>: Evo(Var("T2"))
        )
      ),
      ""
    ),
    Const("exp", Qualified(Scheme(Double =>: Double =>: Double)), ""),
    Const("inverse", Qualified(Scheme(List("T"), Var("T") =>: Var("T"))), ""),
    Const(
      "until",
      Qualified(Scheme(List("T"), Evo(Var("T")) =>: (Var("T") =>: Bool) =>: Evo(Var("T")))),
      ""
    ),
    Const(
      "smoothstep",
      Qualified(Scheme(Double =>: Double =>: Double =>: Double)),
      ""
    ),
    Const("uniform", Qualified(Scheme(Double =>: Double =>: Evo(Double))), ""),
    Const(
      "filter",
      Qualified(Scheme(List("T"), Evo(Var("T")) =>: (Var("T") =>: Bool) =>: Evo(Var("T")))),
      ""
    ),
    Const("sign", Qualified(Scheme(Double =>: Double)), ""),
    Const(
      "mapwithderivative",
      Qualified(
        List(
          Predicate("Add", List(Var("T1"), Var("T1"), Var("T1"))),
          Predicate("Invertible", List(Var("T1")))
        ),
        Scheme(
          List("T1", "T2"),
          Evo(Var("T1")) =>: (Var("T1") =>: Var("T1") =>: Var("T2")) =>: Evo(Var("T2"))
        )
      ),
      ""
    ),
    Const("const", Qualified(Scheme(List("T"), Var("T") =>: Evo(Var("T")))), ""),
    Const(
      "integrate",
      Qualified(Scheme(List("T"), Var("T") =>: Evo(Var("T")) =>: Evo(Var("T")))),
      ""
    ),
    Const(
      "grouped",
      Qualified(Scheme(List("T"), Evo(Var("T")) =>: Integer =>: Evo(Lst(Var("T"))))),
      ""
    ),
    Const("@TPoint", Qualified(Scheme(Evo(Double) =>: Evo(Double) =>: Evo(TPoint))), ""),
    Const(
      "take",
      Qualified(Scheme(List("T"), Evo(Var("T")) =>: Integer =>: Evo(Var("T")))),
      ""
    ),
    Const("norm", Qualified(Scheme(TPoint =>: Double)), ""),
    Const("floor", Qualified(Scheme(Double =>: Integer)), ""),
    Const(
      "range",
      Qualified(Scheme(Double =>: Double =>: Double =>: Evo(Double))),
      ""
    ),
    Const(
      "while",
      Qualified(Scheme(List("T"), Evo(Var("T")) =>: (Var("T") =>: Bool) =>: Evo(Var("T")))),
      ""
    ),
    Const("not", Qualified(Scheme(Bool =>: Bool)), ""),
    Const(
      "iterate2",
      Qualified(
        List(),
        Scheme(
          List("T"),
          (Var("T") =>: Var("T") =>: Var("T")) =>: Var("T") =>: Var("T") =>: Evo(Var("T"))
        )
      ),
      ""
    ),
    Const(
      "multiply",
      Qualified(
        List(Predicate("Mult", List(Var("A"), Var("B"), Var("C")))),
        Scheme(List("A", "B", "C"), Var("A") =>: Var("B") =>: Var("C"))
      ),
      ""
    ),
    Const("fromlist", Qualified(Scheme(List("T"), Lst(Var("T")) =>: Evo(Var("T")))), ""),
    Const("abs", Qualified(Scheme(Double =>: Double)), ""),
    Const(
      "solve2",
      Qualified(
        List(),
        Scheme(
          List("T"),
          Evo(Var("T") =>: Var("T") =>: Var("T")) =>: Var("T") =>: Var("T") =>: Evo(Var("T"))
        )
      ),
      ""
    ),
    Const("uniformchoice", Qualified(Scheme(List("T"), Lst(Var("T")) =>: Evo(Var("T")))), ""),
    Const("sin", Qualified(Scheme(Double =>: Double)), ""),
    Const(
      "uniformfrom",
      Qualified(Scheme(List("T"), Integer =>: Evo(Var("T")) =>: Evo(Var("T")))),
      ""
    ),
    Const(
      "add",
      Qualified(
        List(Predicate("Add", List(Var("A"), Var("B"), Var("C")))),
        Scheme(List("A", "B", "C"), Var("A") =>: Var("B") =>: Var("C"))
      ),
      (p: AdditiveInst[Any, Any, Any]) => (x: Any) => (y: Any) => MaterializeAddition(p.add)(x, y)
    ),
    Const("@polar", Qualified(Scheme(Evo(Double) =>: Evo(Double) =>: Evo(TPoint))), ""),
    Const(
      "eq",
      Qualified(
        List(Predicate("Comp", List(Var("T")))),
        Scheme(List("T"), Var("T") =>: Var("T") =>: Bool)
      ),
      ""
    ),
    Const(
      "neq",
      Qualified(
        List(Predicate("Eq", List(Var("T")))),
        Scheme(List("T"), Var("T") =>: Var("T") =>: Bool)
      ),
      ""
    ),
    Const("parallel", Qualified(Scheme(List("T"), Evo(Evo(Var("T"))) =>: Evo(Var("T")))), ""),
    Const("versor", Qualified(Scheme(TPoint =>: TPoint)), ""),
    Const(
      "derive",
      Qualified(
        List(
          Predicate("Add", List(Var("T"), Var("T"), Var("T"))),
          Predicate("Invertible", List(Var("T")))
        ),
        Scheme(List("T"), Evo(Var("T")) =>: Evo(Var("T")))
      ),
      ""
    ),
    Const(
      "zipwith",
      Qualified(
        List(),
        Scheme(
          List("T1", "T2", "T3"),
          Evo(Var("T1")) =>: Evo(Var("T2")) =>: (Var("T1") =>: Var("T2") =>: Var("T3")) =>: Evo(Var("T3"))
        )
      ),
      ""
    )
  )
}
