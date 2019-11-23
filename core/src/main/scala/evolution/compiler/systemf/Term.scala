package evolution.compiler.systemf

import evolution.compiler.phases.typer.Matchable
import evolution.compiler.phases.typer.model.Substitution
import evolution.compiler.systemf.QType.{=>:, Arrow, Forall, Qualified, Simple}
import evolution.compiler.types.{Type, TypeClassInstance}
import evolution.compiler.types.TypeClasses.Predicate

sealed trait Term[T] {
  def tpe: QType[T]
}

object Term {
  case class Var[T](name: String, tpe: QType[T]) extends Term[T]

  case class Integer(n: Int) extends Term[QType.Simple] {
    val tpe = QType(Type.Integer)
  }

  case class Let[A, B](varName: String, term: Term[A], in: Term[B]) extends Term[B] {
    override def tpe: QType[B] = in.tpe
  }

  case class Lambda[A, B](varName: String, varType: QType[A], term: Term[B]) extends Term[Arrow[A, B]] {
    val tpe: QType[Arrow[A, B]] = varType =>: term.tpe
  }

  case class App[A, B](f: Term[A =>: B], x: Term[A]) extends Term[B] {
    val tpe: QType[B] = f.tpe.as.to
  }

  case class TLambda[T](typeName: String, term: Term[T]) extends Term[Forall[T]] {
    val tpe: QType[Forall[T]] = Forall(typeName, term.tpe)
  }

  case class TApp[T](f: Term[Forall[T]], x: Type) extends Term[T] {
    val tpe: QType[T] = Substitution(f.tpe.as.typeVar -> x).substitute(f.tpe.as.tpe)
  }

  case class PLambda[T](pVar: Predicate, term: Term[T]) extends Term[Qualified[T]] {
    override def tpe: QType[Qualified[T]] = Qualified(pVar, term.tpe)
  }

  case class PApp[T](f: Term[Qualified[T]], instance: TypeClassInstance) extends Term[T] {
    override def tpe: QType[T] = Matchable.tryMatch(f.tpe.as.predicate, instance.predicate) match {
      case Some(subst) => subst.substitute(f.tpe.as.tpe)
      case None        => throw new Exception("Incompatible Predicate Application") // Can we avoid this?
    }
  }

  object App2 {
    def apply[A, B, C](f: Term[A =>: B =>: C], a: Term[A], b: Term[B]): Term[C] =
      App(App(f, a), b)
  }
}
