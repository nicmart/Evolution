package evolution.compilertree.phases.parsing

import fastparse.WhitespaceApi

object ParserConfig {
  import fastparse.all._
  val whitespacesChars = Seq[Seq[Char]](" ", "\n", "\r", "\t")
  val comment = P("//" ~/ ElemPred.create(_ != '\n', false).rep)
  val whitespaces = P(comment | CharIn(whitespacesChars: _*)).rep
  val White = WhitespaceApi.Wrapper {
    NoTrace(ParserConfig.whitespaces)
  }
}
