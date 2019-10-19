package evolution.compiler.phases.parser

import fastparse._

object ParserConfig {
  //import fastparse.NoWhitespace._
  val whitespacesChars = " \n\r\t"

  implicit val whitespace = { implicit ctx: ParsingRun[_] =>
    ScalaWhitespace.whitespace(ctx)
  }

  def whitespaces[_: P]: P[Unit] = whitespace(implicitly[ParsingRun[_]])
}
