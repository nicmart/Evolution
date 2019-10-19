package evolution.logging
import pprint.PPrinter.Color
import evolution.compiler.phases.typer.model.Assignment
import pprint.Tree

trait Logger {
  def log(any: Any): Unit
}

object ColorPPrinterLogger extends Logger {
  private val pprinter = Color.copy(additionalHandlers = {
    case Assignment(from, to) => Tree.Infix(Color.treeify(from), "->", Color.treeify(to))
  })

  def log(any: Any): Unit = pprinter.pprintln(any, height = Int.MaxValue)
}

object NoOpLogger extends Logger {
  def log(any: Any): Unit = ()
}
