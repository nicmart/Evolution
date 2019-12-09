package evolution.compiler.phases.typer.config

import evolution.compiler.expression.typeclass.Comparable
import evolution.compiler.impl.evaluation
import evolution.compiler.impl.evaluation._
import evolution.compiler.types.Type.{Double, Integer, Bool, Evo, Lst, Arrow, Var, Scheme}
import evolution.compiler.types.Type.{Point => TPoint}
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
      "inrect",
      Qualified(List(), Scheme(List(), TPoint =>: TPoint =>: TPoint =>: Bool)),
      (topLeft: Point) => (bottomRight: Point) => (p: Point) => p.inRectangle(topLeft, bottomRight)
    ),
    Const("or", Qualified(List(), Scheme(List(), Bool =>: Bool =>: Bool)), ""),
    Const(
      "roll",
      Qualified(List(), Scheme(List("T"), Evo(Var("T") =>: Var("T")) =>: Var("T") =>: Evo(Var("T")))),
      ""
    ),
    Const(
      "iterate",
      Qualified(List(), Scheme(List("T"), (Var("T") =>: Var("T")) =>: Var("T") =>: Evo(Var("T")))),
      ""
    ),
    Const("flatten", Qualified(List(), Scheme(List("T"), Evo(Evo(Var("T"))) =>: Evo(Var("T")))), ""),
    Const(
      "solve1",
      Qualified(List(), Scheme(List("T"), Evo(Var("T") =>: Var("T")) =>: Var("T") =>: Evo(Var("T")))),
      ""
    ),
    Const("x", Qualified(List(), Scheme(List(), TPoint =>: Double)), ""),
    Const("pi", Qualified(List(), Scheme(List(), Double)), ""),
    Const("TPoint", Qualified(List(), Scheme(List(), Double =>: Double =>: TPoint)), ""),
    Const(
      "octavenoise",
      Qualified(List(), Scheme(List(), Evo(Integer =>: Double =>: TPoint =>: Double))),
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
    Const("empty", Qualified(List(), Scheme(List("T"), Var("T"))), ""),
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
    Const("polar", Qualified(List(), Scheme(List(), Double =>: Double =>: TPoint)), ""),
    Const("mod", Qualified(List(), Scheme(List(), Double =>: Double =>: Double)), ""),
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
    Const("noise", Qualified(List(), Scheme(List(), Evo(TPoint =>: Double))), ""),
    Const("y", Qualified(List(), Scheme(List(), TPoint =>: Double)), ""),
    Const("if", Qualified(List(), Scheme(List("T"), Bool =>: Var("T") =>: Var("T") =>: Var("T"))), ""),
    Const(
      "uniformdiscrete",
      Qualified(List(), Scheme(List(), Double =>: Double =>: Double =>: Evo(Double))),
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
    Const("normal", Qualified(List(), Scheme(List(), Double =>: Double =>: Evo(Double))), ""),
    Const("fix", Qualified(List(), Scheme(List("T"), (Var("T") =>: Var("T")) =>: Var("T"))), ""),
    Const(
      "concat",
      Qualified(List(), Scheme(List("T"), Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T")))),
      ""
    ),
    Const(
      "cons",
      Qualified(List(), Scheme(List("T"), Var("T") =>: Evo(Var("T")) =>: Evo(Var("T")))),
      ""
    ),
    Const("todbl", Qualified(List(), Scheme(List(), Integer =>: Double)), ""),
    Const("div", Qualified(List(), Scheme(List(), Double =>: Double =>: Double)), ""),
    Const(
      "flatmap",
      Qualified(
        List(),
        Scheme(List("T1", "T2"), Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2")))
      ),
      ""
    ),
    Const("cos", Qualified(List(), Scheme(List(), Double =>: Double)), ""),
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
    Const("exp", Qualified(List(), Scheme(List(), Double =>: Double =>: Double)), ""),
    Const("inverse", Qualified(List(), Scheme(List("T"), Var("T") =>: Var("T"))), ""),
    Const(
      "until",
      Qualified(List(), Scheme(List("T"), Evo(Var("T")) =>: (Var("T") =>: Bool) =>: Evo(Var("T")))),
      ""
    ),
    Const(
      "smoothstep",
      Qualified(List(), Scheme(List(), Double =>: Double =>: Double =>: Double)),
      ""
    ),
    Const("uniform", Qualified(List(), Scheme(List(), Double =>: Double =>: Evo(Double))), ""),
    Const(
      "filter",
      Qualified(List(), Scheme(List("T"), Evo(Var("T")) =>: (Var("T") =>: Bool) =>: Evo(Var("T")))),
      ""
    ),
    Const("sign", Qualified(List(), Scheme(List(), Double =>: Double)), ""),
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
    Const("const", Qualified(List(), Scheme(List("T"), Var("T") =>: Evo(Var("T")))), ""),
    Const(
      "integrate",
      Qualified(List(), Scheme(List("T"), Var("T") =>: Evo(Var("T")) =>: Evo(Var("T")))),
      ""
    ),
    Const(
      "grouped",
      Qualified(List(), Scheme(List("T"), Evo(Var("T")) =>: Integer =>: Evo(Lst(Var("T"))))),
      ""
    ),
    Const("@TPoint", Qualified(List(), Scheme(List(), Evo(Double) =>: Evo(Double) =>: Evo(TPoint))), ""),
    Const(
      "take",
      Qualified(List(), Scheme(List("T"), Evo(Var("T")) =>: Integer =>: Evo(Var("T")))),
      ""
    ),
    Const("norm", Qualified(List(), Scheme(List(), TPoint =>: Double)), ""),
    Const("floor", Qualified(List(), Scheme(List(), Double =>: Integer)), ""),
    Const(
      "range",
      Qualified(List(), Scheme(List(), Double =>: Double =>: Double =>: Evo(Double))),
      ""
    ),
    Const(
      "while",
      Qualified(List(), Scheme(List("T"), Evo(Var("T")) =>: (Var("T") =>: Bool) =>: Evo(Var("T")))),
      ""
    ),
    Const("not", Qualified(List(), Scheme(List(), Bool =>: Bool)), ""),
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
    Const("fromlist", Qualified(List(), Scheme(List("T"), Lst(Var("T")) =>: Evo(Var("T")))), ""),
    Const(
      "lessthan",
      Qualified(
        List(Predicate("Comp", List(Var("T")))),
        Scheme(List("T"), Var("T") =>: Var("T") =>: Bool)
      ),
      ""
    ),
    Const(
      "lessthanorequal",
      Qualified(
        List(Predicate("Comp", List(Var("T")))),
        Scheme(List("T"), Var("T") =>: Var("T") =>: Bool)
      ),
      ""
    ),
    Const("abs", Qualified(List(), Scheme(List(), Double =>: Double)), ""),
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
    Const("uniformchoice", Qualified(List(), Scheme(List("T"), Lst(Var("T")) =>: Evo(Var("T")))), ""),
    Const("sin", Qualified(List(), Scheme(List(), Double =>: Double)), ""),
    Const(
      "uniformfrom",
      Qualified(List(), Scheme(List("T"), Integer =>: Evo(Var("T")) =>: Evo(Var("T")))),
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
    Const("@polar", Qualified(List(), Scheme(List(), Evo(Double) =>: Evo(Double) =>: Evo(TPoint))), ""),
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
    Const("parallel", Qualified(List(), Scheme(List("T"), Evo(Evo(Var("T"))) =>: Evo(Var("T")))), ""),
    Const("and", Qualified(List(), Scheme(List(), Bool =>: Bool =>: Bool)), ""),
    Const(
      "greaterthanorequal",
      Qualified(
        List(Predicate("Comp", List(Var("T")))),
        Scheme(List("T"), Var("T") =>: Var("T") =>: Bool)
      ),
      ""
    ),
    Const("versor", Qualified(List(), Scheme(List(), TPoint =>: TPoint)), ""),
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
