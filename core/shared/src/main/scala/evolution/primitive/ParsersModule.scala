package evolution.primitive
import fastparse.all
import evolution.primitive.algebra.parser.ParserConfig.White._
import evolution.primitive.algebra.parser.ParserConfig.whitespaces
import fastparse.noApi._

trait ParsersModule[F[_]] { self: WithAst[F] =>
  import ast._

  object Parsers {
    lazy val parser: Parser[AST] =
      P(whitespaces ~ precedence0 ~ End)

    lazy val precedence0: Parser[AST] =
      P(lambdaOrLet | precedence1)

    lazy val precedence1Ops: Parser[PredefinedFunction] =
      P("+").map(_ => PredefinedFunction.Add)

    lazy val precedence1: Parser[AST] =
      P(precedence2 ~ (precedence1Ops ~/ precedence2).rep).map {
        case (head, tail) => evalAssocBinaryOp(head, tail.toList)
      }

    lazy val precedence2Ops: Parser[PredefinedFunction] =
      P("*").map(_ => PredefinedFunction.Multiply) | P("/").map(_ => PredefinedFunction.Div)

    lazy val precedence2: Parser[AST] =
      P(precedence3 ~ (precedence2Ops ~/ precedence3).rep).map {
        case (head, tail) => evalAssocBinaryOp(head, tail.toList)
      }

    lazy val precedence3Ops: Parser[PredefinedFunction] =
      P("^").map(_ => PredefinedFunction.Exp)

    lazy val precedence3: Parser[AST] =
      P(appOrFactor ~ (precedence3Ops ~/ appOrFactor).rep).map {
        case (head, tail) => evalAssocBinaryOp(head, tail.toList)
      }

    lazy val factor: Parser[AST] =
      P(("(" ~ precedence0 ~ ")") | number | unaryPrefixOp | variable | let | funcCall)

    lazy val appOrFactor: Parser[AST] =
      P(factor ~ ("(" ~/ args ~ ")").?).map {
        case (f, None)       => f
        case (f, Some(args)) => evalApp(f, args)
      }

    lazy val number: Parser[AST.Number] =
      numbers.doubleLiteral.map(AST.Number(_))

    lazy val variable: Parser[AST.Var] =
      P("$" ~~ identifier).map(AST.Var(_))

    lazy val funcCall: Parser[AST] =
      func0Call | P(functionName ~ "(" ~/ args ~ ")").map { case (func, args) => AST.FuncCall(func, args) }

    // Functions with 0 arity
    lazy val func0Call: Parser[AST] =
      function0Name.map(predefinedFunc => AST.FuncCall(predefinedFunc, Nil))

    lazy val unaryPrefixOp: Parser[AST] =
      P("-" ~ factor).map(e => AST.FuncCall(PredefinedFunction.Inverse, List(e)))

    lazy val lambdaOrLet: Parser[AST] = {
      def lambdaTail(id: String): Parser[AST] = P(whitespaces ~ "->" ~/ precedence0).map { expr =>
        AST.Lambda(AST.Var(id), expr)
      }
      def letTail(id: String): Parser[AST] =
        P(whitespaces ~ "=" ~/ precedence0 ~ "in" ~/ precedence0).map {
          case (value, body) =>
            AST.Let(AST.Var(id), value, body)
        }
      def tail(id: String): Parser[AST] = lambdaTail(id) | letTail(id)

      identifier.flatMap(tail)
    }

    lazy val let: Parser[AST] = P("let(" ~/ identifier ~ "," ~ precedence0 ~ "," ~ precedence0 ~ ")").map {
      case (id, value, in) =>
        AST.Let(AST.Var(id), value, in)
    }

    lazy val args: Parser[List[AST]] =
      P(precedence0 ~ ("," ~ args).?).map { case (head, tail) => head :: tail.getOrElse(Nil) }

    lazy val identifier: Parser[String] = (alpha ~~ alphaNum.repX(1).?).!

    lazy val functionName: Parser[PredefinedFunction] = identifier
      .filter(id => PredefinedFunction.lowerCaseNamesToValuesMap.isDefinedAt(id.toLowerCase))
      .map(PredefinedFunction.withNameInsensitive)

    lazy val function0Name: Parser[PredefinedFunction] =
      functionName.filter(PredefinedFunction.functions0.contains)

    lazy val alpha: Parser[Unit] = P(CharIn('a' to 'z') | CharIn('A' to 'Z'))
    lazy val alphaNum: Parser[Unit] = P(CharIn('0' to '9') | alpha)

    def evalAssocBinaryOp(head: AST, tail: List[(PredefinedFunction, AST)]): AST =
      tail match {
        case Nil                        => head
        case (op, tailHead) :: tailTail => AST.FuncCall(op, List(head, evalAssocBinaryOp(tailHead, tailTail)))
      }

    def evalApp(f: AST, args: List[AST]): AST =
      args match {
        case Nil                => f
        case argHead :: argTail => evalApp(AST.FuncCall(PredefinedFunction.App, List(f, argHead)), argTail)
      }

    object numbers {

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
}
