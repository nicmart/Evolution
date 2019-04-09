package evolution.language

import org.scalatest.{ FreeSpec, Matchers }

class ExperimentalInterpreterModuleSpec extends FreeSpec with Matchers {
  val module = new ExperimentalInterpreterModule {}
  import module._

  "asdasd" - {
    "asdasd" in {
      Interpreter[Int, Int]
    }
  }
}
