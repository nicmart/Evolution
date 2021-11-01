package evolution.compiler.phases.parser

// TODO Too OOP 😂
class ParserFailure(index: Int, inputLines: List[String]) extends Throwable:
  private val lineAndColumn = findLineAndColumn(inputLines, index)
  val lineNumber: Int = Math.min(lineAndColumn._1, inputLines.length - 1)
  private val line: String = inputLines(lineNumber)
  private val columnNumber: Int = lineAndColumn._2

  private val columnIndicator: String = (" " * (line.length + 1)).updated(columnNumber, '^')

  def message: String =
    s"""Parsing failed at line ${lineNumber + 1}, column ${columnNumber + 1}:
       |$line
       |$columnIndicator""".stripMargin

  override def getMessage: String = message

  private def findLineAndColumn(lines: List[String], index: Int): (Int, Int) =
    lines match
      case head :: _ if index <= head.length => (0, index)
      case head :: tail =>
        val (l, c) = findLineAndColumn(tail, index - head.length - 1)
        (l + 1, c)
      case _ => (0, 0)
