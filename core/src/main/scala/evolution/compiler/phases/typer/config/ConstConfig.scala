package evolution.compiler.phases.typer.config

import evolution.compiler.impl.evaluation.MaterializeAddition
import evolution.compiler.types.Type.{Scheme, Var}
import evolution.compiler.types.TypeClassInstance.AdditiveInst
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}

object ConstConfig {
  val constants: List[Const] = List(
    Const(
      "add",
      Qualified(
        List(Predicate("Add", List(Var("A"), Var("B"), Var("C")))),
        Scheme(List("A", "B", "C"), Var("A") =>: Var("B") =>: Var("C"))
      ),
      (p: AdditiveInst[Any, Any, Any]) => (x: Any) => (y: Any) => MaterializeAddition(p.add)(x, y)
    )
  )
}
