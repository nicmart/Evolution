package evolution.compiler.systemf

import org.scalatest.FreeSpec
import Term._
import QType.TVar
import evolution.compiler.types.Type

class TermTest extends FreeSpec {
  "foral a. a -> a = identity" in {
    val polyIdentity = TLambda("X", Lambda("x", TVar("X"), Var("x", TVar("X"))))

    App(TApp(polyIdentity, Type.Integer), Integer(1), QType(Type.Integer))
  }
}
