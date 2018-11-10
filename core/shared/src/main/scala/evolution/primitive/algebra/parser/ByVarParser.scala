package evolution.primitive.algebra.parser

import cats.{Applicative, Defer, MonoidK}
import evolution.primitive.algebra.parser.ByVarParser.{Or, Prefixed, Pure, Raw, debugParser}
import fastparse.noApi
import fastparse.noApi.Parser
import fastparse.parsers.Combinators.{Either, Logged}
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi._

import scala.collection.immutable.SortedMap

sealed trait ByVarParser[T] {
  // TODO do not expose parser
  def parser(vars: List[String]): Parser[T]
  def flatMap[B](f: T => ByVarParser[B]): ByVarParser[B]

  def map[B](f: T => B): ByVarParser[B] = flatMap(f andThen Pure.apply)

  def logged(msg: String): ByVarParser[T] = this match {
    case ByVarParser.Fail() => this
    case Pure(t) => this
    case Raw(f, label) => Raw(vars => debugParser(s"Raw: $msg ($vars)", parser(vars)), label)
    case Prefixed(_, _) => this
    case Or(_) => this
  }

  def pushVar(varname: String): ByVarParser[T] = this match {
    case ByVarParser.Fail() => this
    case Pure(t) => this
    case Raw(f, label) => Raw(vars => f(varname :: vars), label)
    case Prefixed(_, _) => this
    case Or(children) => Or(children.map(_.pushVar(varname)))
  }

  def popVar: ByVarParser[T] = this match {
    case ByVarParser.Fail() => this
    case Pure(t) => this
    case Raw(f, label) => Raw(vars => f(vars.drop(1)), label)
    case Prefixed(prefix, next) => Prefixed(prefix, next.popVar)
    case Or(children) => Or(children.map(_.popVar))
  }
}

object ByVarParser {
  type ByVarParserK[R[_], T] = ByVarParser[R[T]]

  case class Fail[T]() extends ByVarParser[T] {
    override def parser(vars: List[String]): Parser[T] = noApi.Fail
    override def flatMap[B](f: T => ByVarParser[B]): ByVarParser[B] = Fail()
  }

  case class Pure[T](t: T) extends ByVarParser[T] {
    override def parser(vars: List[String]): Parser[T] = noApi.PassWith(t)
    override def flatMap[B](f: T => ByVarParser[B]): ByVarParser[B] = f(t)
  }

  case class Raw[T](f: List[String] => Parser[T], label: String = "") extends ByVarParser[T] {
    override def parser(vars: List[String]): Parser[T] = f(vars)
    override def flatMap[B](g: T => ByVarParser[B]): ByVarParser[B] =
      Raw(vars => f(vars).flatMap(t => g(t).parser(vars)), s"$label flatMapped")
    override def toString: String = s"Raw($label)"
  }

  sealed abstract case class Prefixed[T](prefix: String, next: ByVarParser[T]) extends ByVarParser[T] {
    // TODO still we can't add a cut after the prefix
    override def parser(vars: List[String]): Parser[T] =
      P(debugParser(s"$prefix ($vars)", prefix) ~/ debugParser(s"Suffix of $prefix ($vars)", next.parser(vars)))
    override def flatMap[B](f: T => ByVarParser[B]): ByVarParser[B] = Prefixed(prefix, next.flatMap(f))
  }

  def debugParser[T](msg: String, p: Parser[T]): Parser[T] =
    Logged(p, msg, println)

  object Prefixed {
    def apply[T](prefix: String, next: ByVarParser[T]): ByVarParser[T] =
      (prefix, next) match {
        case ("", _) => next
        case (_, Fail()) => Fail()
        case _ => new Prefixed(prefix, next) {}
      }
  }

  sealed abstract case class Or[T](parsers: List[ByVarParser[T]]) extends ByVarParser[T] {
    override def parser(vars: List[String]): Parser[T] =
      debugParser(
        s"OR (${vars.mkString(", ")}) of (${parsers.mkString(", ")})",
        Either(Either.flatten(parsers.map(_.parser(vars)).toVector): _*)
      )
    override def flatMap[B](f: T => ByVarParser[B]): ByVarParser[B] = Or(parsers.map(_.flatMap(f)))
  }

  object Or {
    def apply[T](parsers: List[ByVarParser[T]]): ByVarParser[T] =
      orOfFlatten(flattenChildren(groupByPrefix(flattenChildren(parsers))))

    private def orOfFlatten[T](parsers: List[ByVarParser[T]]): ByVarParser[T] = parsers match {
      case Nil => Fail()
      case head :: Nil => head
      case _ => new Or(parsers) {}
    }

    private def flattenChildren[T](parsers: List[ByVarParser[T]]): List[ByVarParser[T]] = {
      parsers.flatMap {
        case Or(innerParsers) => innerParsers
        case Fail() => Nil
        case parser => List(parser)
      }
    }

    def groupByPrefix[T](parsers: List[ByVarParser[T]]): List[ByVarParser[T]] = {
      val sortedMap: Map[String, List[ByVarParser[T]]] = SortedMap.empty
      parsers
        .foldLeft(sortedMap) { (map, parser) =>
          val (prefix, suffixParser) = split(parser)
          val current = map.getOrElse(prefix, Nil)
          map.updated(prefix, current :+ suffixParser)
        }
        .map { case (prefix, ps) => Prefixed(prefix, orOfFlatten(flattenChildren(ps))) }
        .toList
    }

    private def split[T](parser: ByVarParser[T]): (String, ByVarParser[T]) = parser match {
      case Prefixed(prefix, p) => (prefix, p)
      case _ => ("", parser)
    }
  }

  val Vars: ByVarParser[List[String]] = Raw(PassWith, "variables")

  implicit def orMonoid[R[_]]: MonoidK[ByVarParserK[R, ?]] = new MonoidK[ByVarParserK[R, ?]] {
    override def empty[A]: ByVarParser[R[A]] =
      Fail()
    override def combineK[A](x: ByVarParser[R[A]], y: ByVarParser[R[A]]): ByVarParser[R[A]] =
      Or(List(x, y))
  }

  implicit def defer[R[_]]: Defer[ByVarParserK[R, ?]] = new Defer[ByVarParserK[R, ?]] {
    override def defer[A](fa: => ByVarParser[R[A]]): ByVarParser[R[A]] =
      Raw(vars => P(fa.parser(vars)), "deferred")
  }
}
