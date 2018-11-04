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
    override def parser(vars: List[String]): noApi.Parser[T] = noApi.Fail
    override def map[B](f: T => B): ByVarParser[B] = Fail()
    override def withVar(varname: String): ByVarParser[T] = Fail()
  }

  case class Raw[T](f: List[String] => Parser[T]) extends ByVarParser[T] {
    override def parser(vars: List[String]): noApi.Parser[T] = f(vars)
    override def map[B](g: T => B): ByVarParser[B] = Raw(vars => f(vars).map(g))
    override def withVar(varname: String): ByVarParser[T] = Raw(vars => f(varname :: vars))

  }

  case class Prefixed[T](prefix: String, next: ByVarParser[T]) extends ByVarParser[T] {
    override def parser(vars: List[String]): noApi.Parser[T] = prefix ~/ next.parser(vars)
    override def map[B](f: T => B): ByVarParser[B] = Prefixed(prefix, next.map(f))
    override def withVar(varname: String): ByVarParser[T] = Prefixed(prefix, next.withVar(varname))
  }

  sealed abstract case class Or[T](parsers: List[ByVarParser[T]]) extends ByVarParser[T] {
    override def parser(vars: List[String]): noApi.Parser[T] =
      Either(Either.flatten(parsers.map(_.parser(vars)).toVector): _*)
    override def map[B](f: T => B): ByVarParser[B] = Or(parsers.map(_.map(f)))
    override def withVar(varname: String): ByVarParser[T] = Or(parsers.map(_.withVar(varname)))
  }

  object Or {
    def apply[T](parsers: List[ByVarParser[T]]): ByVarParser[T] =
      if (parsers.nonEmpty) new Or(flatten(parsers)) {} else Fail()

    def flatten[T](parsers: List[ByVarParser[T]]): List[ByVarParser[T]] =
      parsers.flatMap {
        case Or(innerParsers) => flatten(innerParsers)
        case Fail() => Nil
        case parser => List(parser)
      }

    def groupByPrefix[T](parsers: List[ByVarParser[T]]): List[ByVarParser[T]] = {
      val sortedMap: Map[String, ByVarParser[T]] = SortedMap.empty[String, ByVarParser[T]]
      parsers
        .foldLeft(sortedMap) { (map, parser) =>
          val (prefix, suffixParser) = split(parser)
          val current = sortedMap.getOrElse(prefix, Fail())
          sortedMap.updated(prefix, Prefixed(prefix, Or(List(current, suffixParser))))
        }
        .values
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
