package evolution.compiler.phases.parsing

import fastparse._

object ParserConfig {
  //import fastparse.NoWhitespace._
  val whitespacesChars = " \n\r\t"

  def whitespaces[_: P] = {
    import NoWhitespace._
    P(comment | CharIn(" \n\r\t")).rep
  }

  private def comment[_: P] = {
    import NoWhitespace._
    P("//" ~/ CharsWhile(_ != '\n', 0))
  }

  implicit val whitespace = { implicit ctx: ParsingRun[_] =>
    NoTrace(whitespaces)
  }
}
