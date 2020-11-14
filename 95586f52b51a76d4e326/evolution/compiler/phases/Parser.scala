package evolution.compiler.phases
import evolution.compiler.phases.parser.ParserFailure
import evolution.compiler.tree.Tree

trait Parser {
  def parse(astString: String): Either[ParserFailure, Tree]
}
