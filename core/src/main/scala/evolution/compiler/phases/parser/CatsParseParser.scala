package evolution.compiler.phases.parser

import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.Parser.{With1, not, string, void}
import evolution.compiler.phases.parser.CatsParserConfig._
import evolution.compiler.phases.parser.CatsPrecedenceGroup.BinaryOperator
import evolution.compiler.phases.{Parser, parser}
import evolution.compiler.tree
import evolution.compiler.tree.Tree
import evolution.compiler.tree.Tree._
import cats.parse.{Numbers, Parser => P, Parser0 => P0}
import CatsParseParser.Ops
import CatsParseParser.POps

import scala.annotation.tailrec

object CatsParseParser extends Parser {
  def parse(astString: String): Either[ParserFailure, Tree] = {
    program
      .parseAll(astString)
      .leftMap(error => {
        error.expected.toList.foreach(println(_))
        new ParserFailure(error.failedAtOffset, astString.split("\n").toList)
      })
  }

  def binaryOperators: List[(String, BinaryOperator)] = allPrecedenceGroups.flatMap(group => group.operators)

  // WIP
  private lazy val program: P[Tree] = expression.surroundedByWhitespaces

  private lazy val expression: P[Tree] =
    P.defer(NonOperandExpressions.nonOperand | precedenceGroups.operand)

  private object NonOperandExpressions {
    val nonOperand: P[Tree] = {
      // Note: a previous solution based on flatMap caused undesired back-tracking
      (identifier.w.soft ~ (lambdaTail | letTail | sampleTail | argsTail)).map {
        case (id, f) => f(id)
      }
    }

    private lazy val lambdaTail: P[String => Tree] = (P.string("->").w *> expression).map { expr =>
      Lambda(_, expr)
    }

    // `a = 2 in` ... and `a == 2` sha re the same prefix, so the cut must be after the negative lookahead
    private lazy val letTail: P[String => Tree] =
      ((P.char('=').w ~ not(P.char('='))).backtrack *> expression.w ~ (P
        .string("in")
        .w *> expression.w))
        .map {
          case (value, body) =>
            Let(_, value, body)
        }

    private lazy val sampleTail: P[String => Tree] =
      (((P.string("<-").void).soft *> expression) ~| (P.string("in").void.soft *> expression))
        .map {
          case (sampling, body) =>
            variable => tree.SpecialSyntax.withFirst(variable -> sampling, body)
        }

    // We need to allow backtracking, since f(x, y) can be a function application in addition to a binding
    // Parse "(a, b, c) = body in expr"
    private lazy val argsTail: P[String => Tree] = {
      val args = P.char('(').w *> nonEmptyCsv(identifier) <* P.char(')')
      val equal = P.char('=').void ~ not(P.char('='))
      val in = P.string("in").void
      val p = (args.w <* equal.w).backtrack.soft ~ expression.w ~ (in.w *> expression.w)

      p.map {
        case ((args, value), body) => name => tree.SpecialSyntax.functionBinding(name, args.toList, value, body)
      }
    }
  }

  private lazy val allPrecedenceGroups = List(
    CatsPrecedenceGroup(
      ">>" -> ((left, right) => App.of(right, left))
    ),
    CatsPrecedenceGroup(
      "||" -> constOp("or")
    ),
    CatsPrecedenceGroup(
      "&&" -> constOp("and")
    ),
    CatsPrecedenceGroup(
      ">=" -> constOp("greaterthanorequal"),
      ">" -> constOp("greaterthan"),
      "<=" -> constOp("lessthanorequal"),
      "<" -> constOp("lessthan")
    ),
    CatsPrecedenceGroup(
      "==" -> constOp("eq"),
      "!=" -> constOp("neq")
    ),
    CatsPrecedenceGroup(
      "+" -> constOp("add"),
      "-" -> constOp("minus")
    ),
    CatsPrecedenceGroup(
      "*" -> constOp("multiply"),
      "/" -> constOp("div"),
      "%" -> constOp("mod")
    ),
    CatsPrecedenceGroup(
      "^" -> constOp("exp")
    )
  )

  private def constOp(name: String)(left: Tree, right: Tree): Tree =
    App.of(Id(name), left, right)

  // Operator groups, order by ascending Precedence
  private lazy val precedenceGroups: CatsPrecedenceGroups = CatsPrecedenceGroups(
    atomicOperand,
    allPrecedenceGroups
  )

  private lazy val factor: P[Tree] = {
    // TODO: *> and <* with whitespaces
    lazy val prefix: P[Tree] =
      (P.char('(').void *> expression <* P.char(')').void) | doubleLit | boolean | unaryPrefixOp | variable | list

    lazy val app: P[Tree] = (prefix ~ (P.char('(') *> nonEmptyArgs <* P.char(')')).?).map {
      case (tree, None)         => tree
      case (f, Some(arguments)) => App(f, arguments)
    }

    (app ~ (P.char('.').void *> variable ~ (P.char('(').void *> nonEmptyArgs <* P.char(')').void).?).rep0).map {
      case (tree, selections) => dotSelection(tree, selections)
    }.surroundedByWhitespaces
  }

  @tailrec
  private def dotSelection(receiver: Tree, selections: List[(Tree, Option[NonEmptyList[Tree]])]): Tree =
    selections match {
      case Nil => receiver
      case (firstMethod, maybeArgs) :: nextSelections =>
        dotSelection(
          App(firstMethod, NonEmptyList(receiver, maybeArgs.fold(List.empty[Tree])(_.toList))),
          nextSelections
        )
    }

  private lazy val atomicOperand: P[Tree] =
    P.defer(specialSyntax | factor)

  private lazy val list: P[Tree] = (P.char('[').void *> args <* P.char(']').void).map(tree.SpecialSyntax.cons)

  private lazy val doubleLit: P[Tree] =
    numbers.doubleLiteral.map(d => if (d % 1 == 0) IntLiteral(d.toInt) else DoubleLiteral(d))

  private lazy val boolean: P[Tree] =
    (P.string("true").map(_ => true) | P.string("false").map(_ => false)).map(Bool)

  private lazy val variable: P[Tree] =
    identifier.map(Id(_)) <* P.pure(())

  private lazy val unaryOps: P[Tree] =
    P.char('-').void.map(_ => Id("inverse")) |
      P.char('!').void.map(_ => Id("not"))

  private lazy val unaryPrefixOp: P[Tree] =
    (unaryOps ~ atomicOperand).map { case (op, e) => App.of(op, e) }

  private lazy val args: P0[List[Tree]] = nonEmptyArgs.map(_.toList) | P.pure(Nil)

  private lazy val nonEmptyArgs: P[NonEmptyList[Tree]] =
    nonEmptyCsv(expression)

  private def nonEmptyCsv[T](p: P[T]): P[NonEmptyList[T]] =
    P.recursive[NonEmptyList[T]](
      self =>
        (p.w ~ (P.char(',').w.void *> self).?).map {
          case (head, tail) => NonEmptyList(head, tail.map(_.toList).getOrElse(Nil))
        }
    )

  private lazy val alpha: P[Unit] = void(P.charWhere(_.toString.matches("[a-zA-Z_]"))).void
  private lazy val alphaNum: P[Unit] = P.charWhere(_.toString.matches("[a-zA-Z0-9_]")).void

  private lazy val identifier: P[String] =
    ((alpha | P.char('@')) ~ alphaNum.rep(1).?).string.map(_.toLowerCase)

  private lazy val specialSyntax: P[Tree] = special.zip | special.product | special.uniformChoice

  private object special {

    lazy val zip: P[Tree] =
      (P.ignoreCase("zip").void.w *> P
        .char('(')
        .w *> nonEmptyCsv(comprehensionBinding).w ~ (P.char(')').w *> P.string("in").w *> expression))
        .map {
          case (bindings, body) => tree.SpecialSyntax.zip(bindings.toList, body)
        }

    lazy val product: P[Tree] =
      (P.ignoreCase("product")
        .w *> P.char('(').w *> nonEmptyCsv(comprehensionBinding).w ~ (P.char(')').w *> P.string("in").w *> expression))
        .map {
          case (bindings, body) => tree.SpecialSyntax.product(bindings.toList, body)
        }

    lazy val uniformChoice: P[Tree] =
      (P.ignoreCase("uniformchoice").w *> P.char('(').w *> nonEmptyArgs.w <* P.char(')'))
        .map(_.toList)
        .map(tree.SpecialSyntax.uniformChoice)

    private lazy val comprehensionBinding: P[(String, Tree)] =
      identifier.w ~ (P.string("<-").w *> expression)
  }

  private object numbers {

    lazy val digit: P[Unit] =
      P.charIn('0' to '9').void

    lazy val floatDigits: P[Unit] =
      (digit.rep0.with1 ~ P.char('.') ~ digit.rep(1).void).void

    lazy val intLiteral: P[Int] =
      (P.char('-').?.with1 ~ digit.rep).string.map(_.toInt)

    // It would be nice to avoid to backtrack on the "-".
    lazy val doubleLiteral: P[Double] =
      (P.char('-').?.soft.with1 ~ (floatDigits.backtrack | digit.rep(1)) ~ exp.?).string.map(_.toDouble)

    lazy val exp: P[Unit] = (P.charIn('E', 'e') ~ P.charIn('+', '-').? ~ digit.rep).void
  }

  implicit class Ops[A](p: P[A]) {
    def ~|[B](other: P0[B]): P[(A, B)] = (p ~ other).backtrack
    def ~~[B](other: P0[B]): P[(A, B)] = (p <* whitespaces.?) ~ other
    def w: P[A] = (p <* whitespaces.?)
    def surroundedByWhitespaces: P[A] = whitespaces.?.with1.soft *> p <* whitespaces.?
  }

  implicit class POps[A](p: P.type) {
    def charInPattern(pattern: String): P[Char] =
      p.charWhere(_.toString.matches(s"[$pattern]"))
  }

  implicit class With1Ops[A](p: With1[A]) {
    def ~|[B](other: P[B]): P[(A, B)] = (p ~ other).backtrack
  }
}

private[parser] object CatsParserConfig {
  //import fastparse.NoWhitespace._
  val whitespaces: P0[Unit] = P.charsWhile(_.isWhitespace).void
}

private[parser] final case class CatsPrecedenceGroups(last: P[Tree], groups: List[CatsPrecedenceGroup]) {
  def operand: P[Tree] =
    groups.foldRight(last) { (group, accParser) =>
      group.parser(accParser)
    }
}

private[parser] final case class CatsPrecedenceGroup(operators: (String, BinaryOperator)*) {
  def parser(next: P[Tree]): P[Tree] = (next ~~ (opsParser ~~ next).rep0).map {
    case (head, tail) => evalAssocBinaryOp(head, tail)
  }

  private def opsParser: P[BinaryOperator] = operators.foldLeft[P[BinaryOperator]](P.fail) {
    case (accParser, (opString, ast)) =>
      accParser | (P.string(opString).soft ~ allowedCharsAfterOp.peek).as(ast)
  }

  @scala.annotation.tailrec
  private def evalAssocBinaryOp(head: Tree, tail: List[(BinaryOperator, Tree)]): Tree =
    tail match {
      case Nil => head
      case (op, tailHead) :: tailTail =>
        evalAssocBinaryOp(op(head, tailHead), tailTail)
    }

  private def allowedCharsAfterOp: P[Unit] = P.charInPattern("[a-zA-Z0-9\\- \n\r()@._]").void
}

object CatsPrecedenceGroup {
  type BinaryOperator = (Tree, Tree) => Tree
}
