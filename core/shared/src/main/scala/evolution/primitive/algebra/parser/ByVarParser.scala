package evolution.primitive.algebra.parser

import cats.{Applicative, Defer, MonoidK}
import fastparse.noApi
import fastparse.noApi.Parser
import fastparse.parsers.Combinators.Either
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi._

sealed trait ByVarParser[T] {
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
    override def parser(vars: List[String]): noApi.Parser[T] = ???
    override def map[B](f: T => B): ByVarParser[B] = ???
    override def withVar(varname: String): ByVarParser[T] =
      ???
  }

  sealed abstract case class Or[T](parsers: List[ByVarParser[T]]) extends ByVarParser[T] {
    override def parser(vars: List[String]): noApi.Parser[T] =
      Either(Either.flatten(parsers.map(_.parser(vars)).toVector): _*)
    override def map[B](f: T => B): ByVarParser[B] = Or(parsers.map(_.map(f)))
    override def withVar(varname: String): ByVarParser[T] = Or(parsers.map(_.withVar(varname)))
  }

  object Or {
    def apply[T](parsers: List[ByVarParser[T]]): Or[T] = new Or(flatten(parsers)) {}

    def flatten[T](parsers: List[ByVarParser[T]]): List[ByVarParser[T]] =
      parsers.flatMap {
        case Or(innerParsers) => flatten(innerParsers)
        case parser => List(parser)
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
