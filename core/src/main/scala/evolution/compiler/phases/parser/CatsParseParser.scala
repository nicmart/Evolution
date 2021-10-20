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
import CatsParseParser.With1Ops

import scala.annotation.tailrec

object CatsParseParser extends Parser {
  def parse(astString: String): Either[ParserFailure, Tree] = {
    program.parseAll(astString).leftMap(error => new ParserFailure(error.failedAtOffset, astString.split("\n").toList))
  }

  def binaryOperators: List[(String, BinaryOperator)] = allPrecedenceGroups.flatMap(group => group.operators)

  // WIP
  private val program: P[Tree] = expression

  private def expression: P[Tree] =
    P.defer(precedenceGroups.operand)
//    NonOperandExpressions.nonOperand | precedenceGroups.operand

  private object NonOperandExpressions {
    def nonOperand: P[Tree] = {
      // Note: a previous solution based on flatMap caused undesired back-tracking
      (identifier ~| (lambdaTail | letTail | sampleTail | argsTail)).map {
        case (id, f) => f(id)
      }
    }

    private def lambdaTail: P[String => Tree] = (whitespaces.with1 ~ P.string("->") *> expression).map { expr =>
      Lambda(_, expr)
    }

    // `a = 2 in` ... and `a == 2` share the same prefix, so the cut must be after the negative lookahead
    private def letTail: P[String => Tree] =
      (((whitespaces.with1 ~| P.char('=') ~| not(P.char('='))).soft *> expression) ~ (P.string("in") *> expression))
        .map {
          case (value, body) =>
            Let(_, value, body)
        }

    private def sampleTail: P[String => Tree] =
      (((whitespaces.with1 ~| P.string("<-").void).soft *> expression) ~| (P.string("in").void.soft *> expression))
        .map {
          case (sampling, body) =>
            variable => tree.SpecialSyntax.withFirst(variable -> sampling, body)
        }

    // We need to allow backtracking, since f(x, y) can be a function application in addition to a binding
    private def argsTail: P[String => Tree] = {

      ((whitespaces.with1 *> P.char('(') *> nonEmptyCsv(identifier)) ~ (P.char(')') *> (P.char('=') *> not(P.char('=')) *> expression)) ~
        (P.string("in") *> expression))
        .map {
          case ((args, value), body) => name => tree.SpecialSyntax.functionBinding(name, args.toList, value, body)
        }
    }
  }

  private def allPrecedenceGroups = List(
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
  private def precedenceGroups: CatsPrecedenceGroups = CatsPrecedenceGroups(
    atomicOperand,
    allPrecedenceGroups
  )

  private def factor: P[Tree] = {
    // TODO: *> and <* with whitespaces
    def prefix: P[Tree] =
      (P.char('(').void *> expression <* P.char(')').void) | doubleLit | boolean | unaryPrefixOp | variable | list

    def app: P[Tree] = (prefix ~ (P.char('(') *> nonEmptyArgs <* P.char(')')).?).map {
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

  private def atomicOperand: P[Tree] =
//    specialSyntax | factor
    P.defer(factor)

  private def list: P[Tree] = (P.char('[').void *> args <* P.char(']').void).map(tree.SpecialSyntax.cons)

  private def doubleLit: P[Tree] =
    numbers.doubleLiteral.map(d => if (d % 1 == 0) IntLiteral(d.toInt) else DoubleLiteral(d))

  private def boolean: P[Tree] =
    (P.string("true").map(_ => true) | P.string("false").map(_ => false)).map(Bool)

  private def variable: P[Tree] =
    identifier.map(Id(_)) <* P.pure(())

  private def unaryOps: P[Tree] =
    P.char('-').void.map(_ => Id("inverse")) |
      P.char('!').void.map(_ => Id("not"))

  private def unaryPrefixOp: P[Tree] =
    (unaryOps ~ atomicOperand).map { case (op, e) => App.of(op, e) }

  private def args: P0[List[Tree]] = nonEmptyArgs.map(_.toList) | P.pure(Nil)

  private def nonEmptyArgs: P[NonEmptyList[Tree]] =
    nonEmptyCsv(expression)

  private def nonEmptyCsv[T](p: => P[T]): P[NonEmptyList[T]] =
    P.recursive[NonEmptyList[T]](
      self =>
        (p.w ~ (P.char(',').w.void *> self).?).map {
          case (head, tail) => NonEmptyList(head, tail.map(_.toList).getOrElse(Nil))
        }
    )

  private def alpha: P[Unit] = void(P.charWhere(_.toString.matches("[a-zA-Z_]"))).void
  private def alphaNum: P[Unit] = P.charWhere(_.toString.matches("[a-zA-Z0-9_]")).void

  private def identifier: P[String] =
    ((alpha | P.char('@')) ~ alphaNum.rep(1).?).string

  private def specialSyntax: P[Tree] = special.zip | special.product | special.uniformChoice

  private object special {

    def zip: P[Tree] =
      (P.string("zip") *> P.char('(') *> nonEmptyCsv(comprehensionBinding) ~ (P.char(')') *> P.string("in") *> expression))
        .map {
          case (bindings, body) => tree.SpecialSyntax.zip(bindings.toList, body)
        }

    def product: P[Tree] =
      (P.string("product") *> P.char('(') *> nonEmptyCsv(comprehensionBinding) ~ (P.char(')') *> P.string("in") *> expression))
        .map {
          case (bindings, body) => tree.SpecialSyntax.product(bindings.toList, body)
        }

    def uniformChoice: P[Tree] =
      (P.string("uniformchoice") *> P.char('(') *> nonEmptyArgs <* P.char(')'))
        .map(_.toList)
        .map(tree.SpecialSyntax.uniformChoice)

    private def comprehensionBinding: P[(String, Tree)] =
      identifier ~ (P.string("<-") *> expression)
  }

  private object numbers {

    def digit: P[Unit] =
      P.charIn('0' to '9').void

    def floatDigits: P[Unit] =
      (digit.rep0.with1 ~ P.char('.') ~ digit.rep(1).void).void

    def intLiteral: P[Int] =
      (P.char('-').?.with1 ~ digit.rep).string.map(_.toInt)

    // It would be nice to avoid to backtrack on the "-".
    def doubleLiteral: P[Double] =
      (P.char('-').?.soft.with1 ~ (floatDigits.backtrack | digit.rep(1)) ~ exp.?).string.map(_.toDouble)

    def exp: P[Unit] = (P.charIn('E', 'e') ~ P.charIn('+', '-').? ~ digit.rep).void
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
