package evolution.compiler.systemf

import evolution.compiler.systemf.Term._
import evolution.compiler.types.Type
import org.scalatest.FreeSpec

class TermTest extends FreeSpec {
  import QType._
  "foral a. a -> a = identity" in {
    val polyIdentity: Term[Forall[Simple =>: Simple]] =
      TLambda("X", Lambda("x", TVar("X"), Var("x", TVar("X"))))

    val x: Term[Simple] = App(TApp(polyIdentity, Type.Integer), Integer(1))
  }
}
