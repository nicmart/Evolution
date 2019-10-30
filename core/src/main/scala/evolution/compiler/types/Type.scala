package evolution.compiler.types

import cats.implicits._
import evolution.geometry.Point
import evolution.materialization.Evolution

sealed trait TypeT[+T] {
  final def =>:[S](from: TypeT[S]): TypeT[S => T] = TypeT.Arrow(from, this)
}

object TypeT {
  case class Var(name: String) extends TypeT[Any] {
    override def toString: String = name
  }
  case object Integer extends TypeT[Int]
  case object Double extends TypeT[Double]
  case object Point extends TypeT[Point]
  case object Bool extends TypeT[Boolean]
  case class Evo[T](inner: TypeT[T]) extends TypeT[Evolution[T]]
  case class Lst[T](inner: TypeT[T]) extends TypeT[List[T]]
  case class Arrow[A, B](from: TypeT[A], to: TypeT[B]) extends TypeT[A => B] {
    override def toString: String = s"$from -> $to"
  }

  def findChilren[T](self: TypeT[T]): List[TypeT[_]] = self match {
    case TypeT.Evo(inner)      => List(inner)
    case TypeT.Lst(inner)      => List(inner)
    case TypeT.Arrow(from, to) => List(from, to)
    case _                     => Nil
  }

  implicit final class TypeTOpsUntyped(val self: TypeT[_]) extends AnyVal {
    def children: List[TypeT[_]] = self match {
      case TypeT.Evo(inner)      => List(inner)
      case TypeT.Lst(inner)      => List(inner)
      case TypeT.Arrow(from, to) => List(from, to)
      case _                     => Nil
    }

    def typeVars: Set[TypeT.Var] = self match {
      case TypeT.Var(name) => Set(TypeT.Var(name))
      case _               => children.flatMap(_.typeVars).toSet
    }

    def typeVarUsages(varName: String): List[TypeT[_]] =
      typeVars.collect {
        case tpe @ TypeT.Var(name) if varName == name => tpe
      }.toList

    def unwrapEvo: Either[String, TypeT[_]] = self match {
      case TypeT.Evo(inner) => inner.asRight
      case _                => s"TypeT $self is not an Evolution type".asLeft
    }

    def unwrapLst: Either[String, TypeT[_]] = self match {
      case TypeT.Lst(inner) => inner.asRight
      case _                => s"TypeT $self is not a Lst type".asLeft
    }
  }
}
