package evolution.primitive.algebra.parser

import fastparse.{ WhitespaceApi, all, core }

object ParserConfig {
  import fastparse.all._
  val whitespaces = CharIn(" ", "\n", "\r", "\t").rep
  val White = WhitespaceApi.Wrapper {
    NoTrace(ParserConfig.whitespaces)
  }
}
