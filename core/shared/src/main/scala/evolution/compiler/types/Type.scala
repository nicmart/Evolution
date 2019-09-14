package evolution.compiler.types

import cats.implicits._
import evolution.geometry.Point
import evolution.materialization.Evolution

sealed trait Type {
  type Out
}

object Type {
  final case class Var(name: String) extends Type {
    type Out = Nothing
    override def toString: String = name
  }
  final case object Integer extends Type { type Out = Int }
  final case object Double extends Type { type Out = Double }
  final case object Point extends Type { type Out = Point }
  final case object Bool extends Type { type Out = Boolean }
  final case class Evo(inner: Type) extends Type { type Out = Evolution[inner.type] }
  final case class Lst(inner: Type) extends Type { type Out = List[inner.type] }
  final case class Arrow(from: Type, to: Type) extends Type {
    type Out = from.type => to.type
    override def toString: String = s"$from -> $to"
  }

  implicit class TypeOps(val self: Type) extends AnyVal {

    final def children: List[Type] = self match {
      case Type.Evo(inner)      => List(inner)
      case Type.Lst(inner)      => List(inner)
      case Type.Arrow(from, to) => List(from, to)
      case _                    => Nil
    }

    final def =>:(from: Type): Type = Type.Arrow(from, self)

    final def typeVars: Set[Type.Var] = self match {
      case Type.Var(name) => Set(Type.Var(name))
      case _              => children.flatMap(_.typeVars).toSet
    }

    final def typeVarUsages(varName: String): List[Type] =
      typeVars.collect {
        case tpe @ Type.Var(name) if varName == name => tpe
      }.toList

    final def unwrapEvo: Either[String, Type] = self match {
      case Type.Evo(inner) => inner.asRight
      case _               => s"Type $self is not an Evolution type".asLeft
    }

    final def unwrapLst: Either[String, Type] = self match {
      case Type.Lst(inner) => inner.asRight
      case _               => s"Type $self is not a Lst type".asLeft
    }
  }
}
