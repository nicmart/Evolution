package evolution.compiler.types

import cats.implicits._

sealed trait Type

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

  object Evo {
    def apply(typeVarName: String): Type = Evo(Type.Var(typeVarName))
  }

  object Lst {
    def apply(typeVarName: String): Type = Lst(Type.Var(typeVarName))
  }

  case class Scheme(vars: List[String], tpe: Type) {
    override def toString: String = if vars.isEmpty then tpe.toString else s"∀ (${vars.mkString(", ")}) $tpe"
    def instantiate(types: List[Type]): Type = replaceVars(tpe, vars.zip(types))
    def freeVars: Set[String] = tpe.typeVars.map(_.name).diff(vars.toSet)
  }

  object Scheme {
    def apply(tpe: Type, vars: String*): Scheme = Scheme(vars.toList, tpe)
  }

  // TODO this is already done with substitutions
  def replaceVars(tpe: Type, substitutions: List[(String, Type)]): Type = {
    tpe match {
      case Type.Lst(inner)      => Type.Lst(replaceVars(inner, substitutions))
      case Type.Arrow(from, to) => Type.Arrow(replaceVars(from, substitutions), replaceVars(to, substitutions))
      case Type.Evo(inner)      => Type.Evo(replaceVars(inner, substitutions))
      case Type.Var(name)       => substitutions.find(_._1 == name).fold(tpe)(_._2)
      case _                    => tpe
    }
  }

  implicit final class StringTypeOps(typeVarName: String) {
    def =>:(from: Type): Type = Type.Arrow(from, Type.Var(typeVarName))
    def =>:(from: String): Type = Type.Arrow(Type.Var(from), Type.Var(typeVarName))
  }

  implicit final class TypeOps(val self: Type) extends AnyVal {
    def =>:(from: Type): Type = Type.Arrow(from, self)
    def =>:(from: String): Type = Type.Arrow(Type.Var(from), self)

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

    def quantify: Scheme = Scheme(typeVars.toList.map(_.name), self)

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
