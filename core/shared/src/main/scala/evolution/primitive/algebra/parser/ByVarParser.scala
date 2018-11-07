package evolution.primitive.algebra.parser

import cats.{Applicative, Defer, MonoidK}
import fastparse.noApi
import fastparse.noApi.Parser
import fastparse.parsers.Combinators.Either
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi._

import scala.collection.immutable.SortedMap

sealed trait ByVarParser[T] {
  // TODO do not expose parser
  def parser(vars: List[String]): Parser[T]
  def map[B](f: T => B): ByVarParser[B]
  def withVar(varname: String): ByVarParser[T]
}

object ByVarParser {
  type ByVarParserK[R[_], T] = ByVarParser[R[T]]

  case class Fail[T]() extends ByVarParser[T] {
    override def parser(vars: List[String]): Parser[T] = noApi.Fail
    override def map[B](f: T => B): ByVarParser[B] = Fail()
    override def withVar(varname: String): ByVarParser[T] = Fail()
  }

  case class Pure[T](t: T) extends ByVarParser[T] {
    override def parser(vars: List[String]): Parser[T] = noApi.PassWith(t)
    override def map[B](f: T => B): ByVarParser[B] = Pure(f(t))
    override def withVar(varname: String): ByVarParser[T] = this
  }

  case class Raw[T](f: List[String] => Parser[T]) extends ByVarParser[T] {
    override def parser(vars: List[String]): Parser[T] = f(vars)
    override def map[B](g: T => B): ByVarParser[B] = Raw(vars => f(vars).map(g))
    override def withVar(varname: String): ByVarParser[T] = Raw(vars => f(varname :: vars))
  }

  sealed abstract case class Prefixed[T](prefix: String, next: ByVarParser[T]) extends ByVarParser[T] {
    // TODO still we can't add a cut after the prefix
    override def parser(vars: List[String]): Parser[T] = P(debugParser(prefix, prefix) ~ next.parser(vars))
    override def map[B](f: T => B): ByVarParser[B] = Prefixed(prefix, next.map(f))
    override def withVar(varname: String): ByVarParser[T] = Prefixed(prefix, next.withVar(varname))
  }

  def debugParser[T](msg: String, p: Parser[T]): Parser[T] =
    p.map(t => { println(s"Parsed $msg"); t })

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
      Either(Either.flatten(parsers.map(_.parser(vars)).toVector): _*)
    override def map[B](f: T => B): ByVarParser[B] = Or(parsers.map(_.map(f)))
    override def withVar(varname: String): ByVarParser[T] = Or(parsers.map(_.withVar(varname)))
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

  implicit def orMonoid[R[_]]: MonoidK[ByVarParserK[R, ?]] = new MonoidK[ByVarParserK[R, ?]] {
    override def empty[A]: ByVarParser[R[A]] =
      Fail()
    override def combineK[A](x: ByVarParser[R[A]], y: ByVarParser[R[A]]): ByVarParser[R[A]] =
      Or(List(x, y))
  }

  implicit def defer[R[_]]: Defer[ByVarParserK[R, ?]] = new Defer[ByVarParserK[R, ?]] {
    override def defer[A](fa: => ByVarParser[R[A]]): ByVarParser[R[A]] =
      Raw(vars => P(fa.parser(vars)))
  }
}
