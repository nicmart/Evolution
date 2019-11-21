package evolution.compiler.systemf

import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Predicate

sealed trait Term {
  def tpe: QType
}

object Term {
  case class Var(name: String, tpe: QType) extends Term
  case class Integer(n: Int, tpe: QType) extends Term

  case class Lambda(varName: String, term: Term, tpe: QType) extends Term
  case class App(f: Term, x: Term, tpe: QType) extends Term

  case class TLambda(typeName: String, term: Term, tpe: QType) extends Term
  case class TApp(f: Term, x: Type, tpe: QType) extends Term

  case class PLambda(pVar: PVar, term: Term, tpe: QType) extends Term
  case class PApp(f: Term, p: Predicate, tpe: QType) extends Term

  case class Let(varName: String, term: Term, in: Term, tpe: QType) extends Term

  object Integer {
    def apply(n: Int): Term = Integer(n, QType(Type.Integer))
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
