package evolution.compiler.types

import cats.implicits._
import evolution.geometry.Point
import evolution.materialization.Evolution

sealed trait Type {
  final def =>:(from: Type): Type = Type.Arrow(from, this)
}

object Type {
  case class Var(name: String) extends Type {
    override def toString: String = name
  }
  case object Integer extends Type
  case object Double extends Type
  case object Point extends Type
  case object Bool extends Type
  case class Evo(inner: Type) extends Type
  case class Lst(inner: Type) extends Type
  case class Arrow(from: Type, to: Type) extends Type {
    override def toString: String = s"$from -> $to"
  }

  def findChilren(self: Type): List[Type] = self match {
    case Type.Evo(inner)      => List(inner)
    case Type.Lst(inner)      => List(inner)
    case Type.Arrow(from, to) => List(from, to)
    case _                    => Nil
  }

  implicit final class TypeOpsUntyped(val self: Type) extends AnyVal {
    def children: List[Type] = self match {
      case Type.Evo(inner)      => List(inner)
      case Type.Lst(inner)      => List(inner)
      case Type.Arrow(from, to) => List(from, to)
      case _                    => Nil
    }

    def typeVars: Set[Type.Var] = self match {
      case Type.Var(name) => Set(Type.Var(name))
      case _              => children.flatMap(_.typeVars).toSet
    }

    def typeVarUsages(varName: String): List[Type] =
      typeVars.collect {
        case tpe @ Type.Var(name) if varName == name => tpe
      }.toList

    def unwrapEvo: Either[String, Type] = self match {
      case Type.Evo(inner) => inner.asRight
      case _               => s"Type $self is not an Evolution type".asLeft
    }

    def unwrapLst: Either[String, Type] = self match {
      case Type.Lst(inner) => inner.asRight
      case _               => s"Type $self is not a Lst type".asLeft
    }
  }
}
