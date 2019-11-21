package evolution.compiler.systemf

import com.sun.tools.javac.code.Type.ForAll
import evolution.compiler.systemf.QType.{Arrow, Forall, Qualified, Simple}
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Predicate

sealed trait Term[T] {
  def tpe: QType[T]
}

object Term {
  case class Var[T](name: String, tpe: QType[T]) extends Term[T]
  case class Integer(n: Int) extends Term[QType.Simple] {
    def tpe = QType(Type.Integer)
  }

  case class Lambda[A, B](varName: String, term: Term[B], tpe: QType[Arrow[A, B]]) extends Term[Arrow[A, B]]
  case class App[A, B](f: Term[Arrow[A, B]], x: Term[A], tpe: QType[B]) extends Term[B]

  case class TLambda[T](typeName: String, term: Term[T], tpe: QType[ForAll[T]]) extends Term[ForAll[T]]
  case class TApp[T](f: Term[ForAll[T]], x: Type, tpe: QType[T]) extends Term[T]

  case class PLambda[T](pVar: PVar, term: Term[T], tpe: QType[Qualified[T]]) extends Term[Qualified[T]]
  case class PApp[T](f: Term[Qualified[T]], p: Predicate, tpe: QType[T]) extends Term[T]

  case class Let[A, B](varName: String, term: Term[A], in: Term[B], tpe: QType[B]) extends Term[B]

  object Integer {
    def apply(n: Int): Term[Simple] = Integer(n, QType(Type.Integer))
  }

  object Lambda {
    def apply(varName: String, varTpe: QType, term: Term): Term =
      Lambda(varName, term, varTpe =>: term.tpe)
  }

  object App {
    def apply(f: Term, x: Term): Term = ??? // Not sure here what to do
  }

  object TLambda {
    def apply(typeName: String, term: Term): Term =
      TLambda(typeName, term, QType.Forall(typeName, term.tpe))
  }

  object TApp {
    def apply(f: Term, x: Type): Term =
      f.tpe match {
        case QType.Forall(typeVar, tpe) => ??? // TODO replace typeVar with x in tpe
        case _                          => f
      }
  }

  implicit class TermOps(term: Term) {
    def withType(newTpe: QType): Term = term match {
      case Var(name, _)               => Var(name, newTpe)
      case Integer(n, _)              => Integer(n, newTpe)
      case Lambda(varName, term, _)   => Lambda(varName, term, newTpe)
      case App(f, x, _)               => App(f, x, newTpe)
      case TLambda(typeName, term, _) => TLambda(typeName, term, newTpe)
      case TApp(f, x, _)              => TApp(f, x, newTpe)
      case PLambda(pVar, term, _)     => PLambda(pVar, term, newTpe)
      case PApp(f, p, _)              => PApp(f, p, newTpe)
      case Let(varName, term, in, _)  => Let(varName, term, in, newTpe)
    }
  }

  case class PVar(id: String, typeVars: List[String])
}
