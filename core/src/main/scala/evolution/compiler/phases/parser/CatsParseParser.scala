package evolution.compiler.phases.parser

import cats.data.NonEmptyList
import cats.implicits.*
import cats.parse.Parser.{not, void}
import cats.parse.{Parser as P, Parser0 as P0}
import evolution.compiler.phases.Parser
import evolution.compiler.phases.parser.Instances.given
import evolution.compiler.phases.parser.Instances.*
import evolution.compiler.phases.parser.PrecedenceGroup.BinaryOperator
import evolution.compiler.tree
import evolution.compiler.tree.Pos
import evolution.compiler.tree.Tree
import evolution.compiler.tree.Tree.*

import scala.annotation.tailrec

object CatsParseParser extends Parser:
  def parse(astString: String): Either[ParserFailure, Tree] =
    program
      .parseAll(astString)
      .leftMap(error => {
        error.expected.toList.foreach(println(_))
        ParserFailure(error.failedAtOffset, astString.split("\n").toList)
      })

  def binaryOperators: List[(String, BinaryOperator)] = allPrecedenceGroups.flatMap(group => group.operators)

  // WIP
  private lazy val program: P[Tree] = expression.surroundedByWhitespaces

  private lazy val expression: P[Tree] =
    (P.defer(NonOperandExpressions.nonOperand | precedenceGroups.operand)
      .mapWithPos((tree, pos) => tree))
      .mapWithPos((tree, pos) => tree.withPos(pos))

  private object NonOperandExpressions:
    val nonOperand: P[Tree] =
      // Note: a previous solution based on flatMap caused undesired back-tracking
      (identifier.w.soft ~ (lambdaTail | letTail | sampleTail | argsTail)).map { case (id, f) =>
        f(id)
      }

    private lazy val lambdaTail: P[String => Tree] = (P.string("->").w *> expression).map { expr =>
      Lambda(_, expr)
    }

    // `a = 2 in` ... and `a == 2` sha re the same prefix, so the cut must be after the negative lookahead
    private lazy val letTail: P[String => Tree] =
      ((P.char('=').w ~ not(P.char('='))).backtrack *> expression.w ~ (P.string("in").w *> expression))
        .map { case (value, body) =>
          Let(_, value, body)
        }

    private lazy val sampleTail: P[String => Tree] =
      (((P.string("<-").void.w).soft *> expression) ~| (P.string("in").void.w.soft *> expression))
        .map { case (sampling, body) =>
          variable => tree.SpecialSyntax.withFirst(variable -> sampling, body)
        }

    // We need to allow backtracking, since f(x, y) can be a function application in addition to a binding
    // Parse "(a, b, c) = body in expr"
    private lazy val argsTail: P[String => Tree] =
      val args = P.char('(').w *> nonEmptyCsv(identifier) <* P.char(')')
      val equal = P.char('=').void ~ not(P.char('='))
      val in = P.string("in").void
      val p = (args.w <* equal.w).backtrack.soft ~ expression.w ~ (in.w *> expression.w)

      p.map { case ((args, value), body) =>
        name => tree.SpecialSyntax.functionBinding(name, args.toList, value, body)
      }

  private lazy val allPrecedenceGroups = List(
    PrecedenceGroup(
      ">>" -> ((left, _, right) => App.of(right, left).withPos(Pos(left.start, right.end)))
    ),
    PrecedenceGroup(
      "||" -> constOp("or")
    ),
    PrecedenceGroup(
      "&&" -> constOp("and")
    ),
    PrecedenceGroup(
      ">=" -> constOp("greaterthanorequal"),
      ">" -> constOp("greaterthan"),
      "<=" -> constOp("lessthanorequal"),
      "<" -> constOp("lessthan")
    ),
    PrecedenceGroup(
      "==" -> constOp("eq"),
      "!=" -> constOp("neq")
    ),
    PrecedenceGroup(
      "+" -> constOp("add"),
      "-" -> constOp("minus")
    ),
    PrecedenceGroup(
      "*" -> constOp("multiply"),
      "/" -> constOp("div"),
      "%" -> constOp("mod")
    ),
    PrecedenceGroup(
      "^" -> constOp("exp")
    )
  )

  private def constOp(name: String)(left: Tree, opPos: Pos, right: Tree): Tree =
    App.of(Id(name, false, opPos), left, right).withPos(Pos(left.start, right.end))

  // Operator groups, order by ascending Precedence
  private lazy val precedenceGroups: PrecedenceGroups = PrecedenceGroups(atomicOperand, allPrecedenceGroups)

  private lazy val factor: P[Tree] =
    // TODO: *> and <* with whitespaces
    lazy val prefix: P[Tree] =
      (P.char('(').void.w *> expression.w <* P.char(')').void) | doubleLit | boolean | unaryPrefixOp | variable | list

    lazy val app: P[Tree] = (prefix.w ~ (P.char('(').w *> nonEmptyArgs.w <* P.char(')')).?).mapWithPos {
      case ((tree, None), pos)         => tree.withPos(pos)
      case ((f, Some(arguments)), pos) => App(f, arguments, pos)
    }

    lazy val selection = P.char('.').w *> variable.w ~ argsInBraces.addPos.w.?
    lazy val argsInBraces: P[NonEmptyList[Tree]] = P.char('(').w *> nonEmptyArgs.w <* P.char(')')

    (app.w ~ selection.rep0).map(dotSelection)

  @tailrec
  private def dotSelection(receiver: Tree, selections: List[(Tree, Option[(NonEmptyList[Tree], Pos)])]): Tree =
    selections match
      case Nil => receiver
      case (firstMethod, None) :: nextSelections =>
        val first =
          App(firstMethod, NonEmptyList.of(receiver)).withPos(Pos(receiver.start, firstMethod.end))
        dotSelection(first, nextSelections)
      case (firstMethod, Some(args, pos)) :: nextSelections =>
        val first =
          App(firstMethod, NonEmptyList(receiver, args.toList)).withPos(Pos(receiver.start, pos.end))
        dotSelection(first, nextSelections)

  private lazy val atomicOperand: P[Tree] =
    P.defer(specialSyntax | factor)

  private lazy val list: P[Tree] = (P.char('[').void.w *> args <* P.char(']').void).map(tree.SpecialSyntax.cons)

  private lazy val doubleLit: P[Tree] =
    numbers.doubleLiteral.mapWithPos((d, pos) => if d % 1 == 0 then IntLiteral(d.toInt, pos) else DoubleLiteral(d, pos))

  private lazy val boolean: P[Tree] =
    (P.string("true").map(_ => true) | P.string("false").map(_ => false)).mapWithPos(Bool)

  private lazy val variable: P[Tree] =
    identifier.mapWithPos((id, pos) => Id(id, false, pos)) <* P.pure(())

  private lazy val unaryOps: P[Tree] =
    P.char('-').onlyPos.map(pos => Id("inverse", false, pos)) |
      P.char('!').onlyPos.map(pos => Id("not", false, pos))

  private lazy val unaryPrefixOp: P[Tree] =
    (unaryOps ~ atomicOperand).mapWithPos { case ((op, e), pos) => App.of(op, e) }

  private lazy val args: P0[List[Tree]] = nonEmptyArgs.map(_.toList) | P.pure(Nil)

  private lazy val nonEmptyArgs: P[NonEmptyList[Tree]] =
    nonEmptyCsv(expression)

  private def nonEmptyCsv[T](p: P[T]): P[NonEmptyList[T]] =
    P.recursive[NonEmptyList[T]](self =>
      (p.w ~ (P.char(',').w.void *> self).?).map { case (head, tail) =>
        NonEmptyList(head, tail.map(_.toList).getOrElse(Nil))
      }
    )

  private lazy val alpha: P[Unit] = void(P.charWhere(_.toString.matches("[a-zA-Z_]"))).void
  private lazy val alphaNum: P[Unit] = P.charWhere(_.toString.matches("[a-zA-Z0-9_]")).void

  private lazy val identifier: P[String] =
    ((alpha | P.char('@')) ~ alphaNum.rep(1).?).string.map(_.toLowerCase)

  private lazy val specialSyntax: P[Tree] = special.zip | special.product | special.uniformChoice

  private object special:
    // We backtrack after the ( because otherwise identifiers starting with one of these will not be parsed
    private def funcStart(name: String): P[Unit] =
      (P.ignoreCase(name).void.w *> P.char('(')).backtrack.w
    lazy val zip: P[Tree] =
      // We backtrack because otherwise zipWith will fail to parse
      (funcStart("zip") *> nonEmptyCsv(comprehensionBinding).w ~ (P
        .char(')')
        .w *> P.string("in").w *> expression))
        .map { case (bindings, body) =>
          tree.SpecialSyntax.zip(bindings.toList, body)
        }

    lazy val product: P[Tree] =
      (funcStart("product") *> nonEmptyCsv(comprehensionBinding).w ~ (P.char(')').w *> P.string("in").w *> expression))
        .map { case (bindings, body) =>
          tree.SpecialSyntax.product(bindings.toList, body)
        }

    lazy val uniformChoice: P[Tree] =
      (funcStart("uniformchoice") *> nonEmptyArgs.w <* P.char(')'))
        .map(_.toList)
        .map(tree.SpecialSyntax.uniformChoice)

    private lazy val comprehensionBinding: P[(String, Tree)] =
      identifier.w ~ (P.string("<-").w *> expression)

  private object numbers:

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

private[parser] object CatsParserConfig:
  val comment: P[Unit] = (P.string("//") ~ P.charWhere(_ != '\n').rep0 ~ P.char('\n').?).void
  val spaces: P[Unit] = P.charsWhile(_.isWhitespace).void
  val whitespaces: P0[Unit] = (comment | spaces).rep0.void
