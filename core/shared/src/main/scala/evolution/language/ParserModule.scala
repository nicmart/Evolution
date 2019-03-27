package evolution.language
import evolution.language.ParserConfig.White._
import evolution.language.ParserConfig.whitespaces
import fastparse.noApi._

trait ParserModule[F[_]] { self: ASTModule[F] with PredefinedConstantsModule[F] =>

  object Parsers {
    def parse(astString: String): Either[ParserFailure, AST] =
      Parsers.parser
        .parse(astString)
        .fold((_, failIndex, extra) => Left(new ParserFailure(failIndex, extra)), (expr, _) => Right(expr))

    def binaryOperators: List[(String, AST)] = precedenceGroups.allOperators

    private val parser: Parser[AST] =
      P(whitespaces ~ ast ~ End)

    private lazy val ast: Parser[AST] =
      P(lambdaOrLet | precedenceGroups.parser)

    // Operator groups, order by ascending Precedence
    private lazy val precedenceGroups: PrecedenceGroups = PrecedenceGroups(
      appOrFactor,
      List(
        PrecedenceGroup(
          "||" -> AST.Identifier(Constant2.Or.entryName)
        ),
        PrecedenceGroup(
          "&&" -> AST.Identifier(Constant2.And.entryName)
        ),
        PrecedenceGroup(
          ">=" -> AST.Identifier(Constant2.GreaterThanOrEqual.entryName),
          ">" -> AST.Identifier(Constant2.GreaterThan.entryName),
          "<=" -> AST.Identifier(Constant2.LessThanOrEqual.entryName),
          "<" -> AST.Identifier(Constant2.LessThan.entryName)
        ),
        PrecedenceGroup(
          "==" -> AST.Identifier(Constant2.Eq.entryName),
          "!=" -> AST.Identifier(Constant2.Neq.entryName)
        ),
        PrecedenceGroup(
          "+" -> AST.Identifier(Constant2.Add.entryName),
          "@+" -> AST.Identifier(Constant2.LiftedAdd.entryName),
          "-" -> AST.Identifier(Constant2.Minus.entryName)
        ),
        PrecedenceGroup(
          "*" -> AST.Identifier(Constant2.Multiply.entryName),
          "@*" -> AST.Identifier(Constant2.LiftedMultiply.entryName),
          "/" -> AST.Identifier(Constant2.Div.entryName),
          "%" -> AST.Identifier(Constant2.Mod.entryName),
        ),
        PrecedenceGroup(
          "^" -> AST.Identifier(Constant2.Exp.entryName)
        )
      )
    )

    private lazy val factor: Parser[AST] =
      P(
        ("(" ~/ ast ~/ ")") | number | boolean | unaryPrefixOp |
          variable | list)

    private lazy val appOrFactor: Parser[AST] =
      P(factor ~ ("(" ~/ nonEmptyArgs ~/ ")").?).map {
        case (f, None)       => f
        case (f, Some(args)) => evalApp(f, args)
      }

    private lazy val list: Parser[AST] = P("[" ~/ args ~/ "]").map(evalList)

    private lazy val number: Parser[AST.Number] =
      numbers.doubleLiteral.map(AST.Number(_))

    private lazy val boolean: Parser[AST.Bool] =
      (P("true").map(_ => true) | P("false").map(_ => false)).map(AST.Bool(_))

    private lazy val variable: Parser[AST.Identifier] =
      P(identifier).map(AST.Identifier(_)) ~/ Pass

    private lazy val unaryOps: Parser[AST.Identifier] =
      P("-").map(_ => AST.Identifier(Constant1.Inverse.entryName)) |
        P("!").map(_ => AST.Identifier(Constant1.Not.entryName))

    private lazy val unaryPrefixOp: Parser[AST] =
      P(unaryOps ~ appOrFactor).map { case (op, e) => AST.App(op, e) }

    private lazy val lambdaOrLet: Parser[AST] = {
      def lambdaTail(id: String): Parser[AST] = P(whitespaces ~ "->" ~/ ast).map { expr =>
        AST.Lambda(id, expr)
      }

      def letTail(id: String): Parser[AST] =
        P(whitespaces ~ "=" ~/ ast ~/ "in" ~/ ast).map {
          case (value, body) =>
            AST.Let(id, value, body)
        }
      def tail(id: String): Parser[AST] = lambdaTail(id) | letTail(id)

      identifier.flatMap(tail)
    }

    private lazy val args: Parser[List[AST]] = P(nonEmptyArgs | PassWith(Nil))

    private lazy val nonEmptyArgs: Parser[List[AST]] =
      P(ast ~ ("," ~ nonEmptyArgs).?).map { case (head, tail) => head :: tail.getOrElse(Nil) }

    private lazy val identifier: Parser[String] =
      ((alpha | CharIn(Seq('@'))) ~~ alphaNum.repX(1).?).!.map(_.toLowerCase)

    private lazy val alpha: Parser[Unit] = P(CharIn('a' to 'z') | CharIn('A' to 'Z'))
    private lazy val alphaNum: Parser[Unit] = P(CharIn('0' to '9') | alpha)

    private def evalApp(f: AST, args: List[AST]): AST =
      args match {
        case Nil                => f
        case argHead :: argTail => evalApp(AST.App(f, argHead), argTail)
      }

    private def evalList(asts: List[AST]): AST = asts match {
      case Nil          => AST.Identifier(Constant0.Empty.entryName)
      case head :: tail => AST.App2(AST.Identifier(Constant2.Cons.entryName), head, evalList(tail))
    }

    private object numbers {

      lazy val digit: Parser[Unit] =
        P(CharIn('0' to '9'))

      lazy val floatDigits: Parser[Unit] =
        P(digit.rep ~~ "." ~~ digit.repX(1))

      lazy val intLiteral: Parser[Int] =
        digit.!.map(_.toInt)

      lazy val doubleLiteral: Parser[String] =
        P("-".? ~~ (floatDigits | digit.repX(1)) ~~ exp.?).!

      lazy val exp: Parser[Unit] = P(CharIn("Ee") ~~ CharIn("+\\-").? ~~ digit.repX(1))
    }
  }

  private[language] case class PrecedenceGroup(operators: (String, AST)*) {
    def parser(next: Parser[AST]): Parser[AST] = P(next ~ (opsParser ~/ next).rep).map {
      case (head, tail) => evalAssocBinaryOp(head, tail.toList)
    }

    private def opsParser: Parser[AST] = operators.foldLeft[Parser[AST]](Fail) {
      case (accParser, (opString, ast)) =>
        accParser | P(opString).map(_ => ast)
    }

    private def evalAssocBinaryOp(head: AST, tail: List[(AST, AST)]): AST =
      tail match {
        case Nil                        => head
        case (op, tailHead) :: tailTail => AST.App(AST.App(op, head), evalAssocBinaryOp(tailHead, tailTail))
      }
  }

  private[language] case class PrecedenceGroups(last: Parser[AST], groups: List[PrecedenceGroup]) {
    def parser: Parser[AST] = groups.foldRight(last) { (group, accParser) =>
      group.parser(accParser)
    }

    def allOperators: List[(String, AST)] = groups.flatMap(group => group.operators)
  }
}

// TODO Too OOP 😂
class ParserFailure(index: Int, extra: Parsed.Failure.Extra[Char, String]) {
  val inputLines: List[String] = extra.input.asInstanceOf[IndexedParserInput].data.split("\n").toList
  private val lineAndColumn = findLineAndColumn(inputLines, index)
  val lineNumber: Int = lineAndColumn._1
  val columnNumber: Int = lineAndColumn._2
  val line: String = inputLines(lineNumber)

  def message: String =
    s"""Parsing failed at line ${lineNumber + 1}:${columnNumber + 1}:
       |$line
       |Expected: ${extra.traced.expected}
       """.stripMargin

  private def findLineAndColumn(lines: List[String], index: Int): (Int, Int) =
    lines match {
      case head :: _ if index <= head.length => (0, index)
      case head :: tail =>
        val (l, c) = findLineAndColumn(tail, index - head.length - 1)
        (l + 1, c)
      case _ => (0, 0)
    }
}
