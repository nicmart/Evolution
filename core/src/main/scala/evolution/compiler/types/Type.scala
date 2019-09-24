package evolution.compiler.types

import cats.implicits._
import evolution.geometry.Point
import evolution.materialization.Evolution

sealed trait TypeT[+T] {
  final def =>:[S](from: TypeT[S]): TypeT[S => T] = TypeT.Arrow(from, this)
}

object TypeT {
  final case class Var(name: String) extends TypeT[Any] {
    override def toString: String = name
  }
  final case object Integer extends TypeT[Int]
  final case object Double extends TypeT[Double]
  final case object Point extends TypeT[Point]
  final case object Bool extends TypeT[Boolean]
  final case class Evo[T](inner: TypeT[T]) extends TypeT[Evolution[T]]
  final case class Lst[T](inner: TypeT[T]) extends TypeT[List[T]]
  final case class Arrow[A, B](from: TypeT[A], to: TypeT[B]) extends TypeT[A => B] {
    override def toString: String = s"$from -> $to"
  }

  final def findChilren[T](self: TypeT[T]): List[TypeT[_]] = self match {
    case TypeT.Evo(inner)      => List(inner)
    case TypeT.Lst(inner)      => List(inner)
    case TypeT.Arrow(from, to) => List(from, to)
    case _                     => Nil
  }

  implicit class TypeTOpsUntyped(val self: TypeT[_]) extends AnyVal {
    final def children: List[TypeT[_]] = self match {
      case TypeT.Evo(inner)      => List(inner)
      case TypeT.Lst(inner)      => List(inner)
      case TypeT.Arrow(from, to) => List(from, to)
      case _                     => Nil
    }

    final def typeVars: Set[TypeT.Var] = self match {
      case TypeT.Var(name) => Set(TypeT.Var(name))
      case _               => children.flatMap(_.typeVars).toSet
    }

    final def typeVarUsages(varName: String): List[TypeT[_]] =
      typeVars.collect {
        case tpe @ TypeT.Var(name) if varName == name => tpe
      }.toList

    final def unwrapEvo: Either[String, TypeT[_]] = self match {
      case TypeT.Evo(inner) => inner.asRight
      case _                => s"TypeT $self is not an Evolution type".asLeft
    }

    final def unwrapLst: Either[String, TypeT[_]] = self match {
      case TypeT.Lst(inner) => inner.asRight
      case _                => s"TypeT $self is not a Lst type".asLeft
    }
  }
}
