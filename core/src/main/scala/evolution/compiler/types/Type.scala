package evolution.compiler.types

import cats.implicits._

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
  case class ForAll(varName: String, tpe: Type) extends Type {
    override def toString: String = s"∀ $varName $tpe"
    def withType(t: Type): Type = replaceVar(tpe, varName, t)
  }

  object ForAll {
    def apply(vars: List[String], tpe: Type): Type = vars match {
      case Nil          => tpe
      case head :: tail => ForAll(head, ForAll(tail, tpe))
    }
  }

  // TODO this is already done with substitutions
  def replaceVar(tpe: Type, varName: String, replaceWith: Type): Type = {
    def replace(tpe: Type): Type = replaceVar(tpe, varName, replaceWith)
    tpe match {
      case Type.ForAll(forallT, body) => // Shadowing
        if (forallT == varName) tpe else Type.ForAll(forallT, replace(body))
      case Type.Lst(inner)      => Type.Lst(replace(inner))
      case Type.Arrow(from, to) => Type.Arrow(replace(from), replace(to))
      case Type.Evo(inner)      => Type.Evo(replace(inner))
      case Type.Var(name)       => if (name == varName) replaceWith else tpe
      case _                    => tpe
    }
  }

  def mapChildren(tpe: Type, f: Type => Type): Type =
    tpe match {
      case Arrow(from, to)      => Arrow(f(from), f(to))
      case Lst(inner)           => Lst(f(inner))
      case ForAll(varName, tpe) => ForAll(varName, f(tpe))
      case Evo(inner)           => Evo(f(inner))
      case _                    => tpe
    }

  implicit final class TypeOpsUntyped(val self: Type) extends AnyVal {
    def children: List[Type] = self match {
      case Type.Evo(inner)      => List(inner)
      case Type.Lst(inner)      => List(inner)
      case Type.Arrow(from, to) => List(from, to)
      case Type.ForAll(_, tpe)  => List(tpe)
      case _                    => Nil
    }

    def mapChildren(f: Type => Type): Type = Type.mapChildren(self, f)

    def transformRec(f: Type => Type): Type = f(self.mapChildren(child => child.transformRec(f)))

    def typeVars: Set[Type.Var] = self match {
      case Type.Var(name) => Set(Type.Var(name))
      case _              => children.flatMap(_.typeVars).toSet
    }

    def quantifiedTypeVars: Set[Type.Var] = self match {
      case ForAll(varName, tpe) => children.flatMap(_.quantifiedTypeVars).toSet + Type.Var(varName)
      case _                    => children.flatMap(_.quantifiedTypeVars).toSet
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
