package evolution.primitive.algebra.parser

import fastparse.{ WhitespaceApi, all, core }

object ParserConfig {
  import fastparse.all._
  val whitespacesChars = Seq[Seq[Char]](" ", "\n", "\r", "\t")
  val comment = P("//" ~/ ElemPred.create(_ != '\n', false).rep)
  val whitespaces = P(comment | CharIn(whitespacesChars: _*)).rep
  val White = WhitespaceApi.Wrapper {
    NoTrace(ParserConfig.whitespaces)
  }
}
