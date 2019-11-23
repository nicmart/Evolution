package evolution.compiler.systemf

import evolution.compiler.expression.typeclass.Additive
import evolution.compiler.systemf.Term._
import evolution.compiler.types.TypeClassInstance.AdditiveInst
import evolution.compiler.types.{Type, TypeClassInstance, TypeClasses}
import evolution.compiler.types.TypeClasses.Predicate
import org.scalatest.FreeSpec

class TermTest extends FreeSpec {
  import QType._
  "foral a. a -> a = identity" in {
    val polyIdentity: Term[Forall[Simple =>: Simple]] =
      TLambda("X", Lambda("x", TVar("X"), Var("x", TVar("X"))))

    val add = Var("add", TVar("X") =>: TVar("X") =>: TVar("X"))

    val double = TLambda(
      "X",
      PLambda(
        Predicate.fromVars("Add", List("X", "X", "X")),
        App2(add, Var("x", TVar("X")), Var("x", TVar("X")))
      )
    )

    val doubleIntegers = PApp(TApp(double, Type.Integer), TypeClassInstance.AdditiveInst(Additive.DoubleDoubleDouble))

    val x: Term[Simple] = App(TApp(polyIdentity, Type.Integer), Integer(1))
  }
}
