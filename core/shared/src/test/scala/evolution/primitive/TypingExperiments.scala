//package evolution.primitive
//import evolution.primitive.Test.Typed.ExType
//
//object Test {
//  object Untyped {
//    sealed trait Type {
//      def ==>(to: Type): Type = Arrow(this, to)
//    }
//    case object Integer extends Type
//    case class Arrow(from: Type, to: Type) extends Type
//
//    sealed trait Term
//    case class Var(name: String) extends Term
//    case class Lam(v: String, `type`: Type, term: Term) extends Term
//    case class App(f: Term, in: Term) extends Term
//    case class Lit(n: Int) extends Term
//    case class Add(a: Term, b: Term) extends Term
//  }
//
//  object Typed {
//    sealed trait Type[A] {
//      def ==>[B](to: Type[B]): Type[A => B] = Arrow(this, to)
//    }
//    case object Integer extends Type[Int]
//    case class Arrow[A, B](from: Type[A], to: Type[B]) extends Type[A => B]
//
//    sealed trait Term[T]
//    case class Var[T](name: String) extends Term[T]
//    case class Lam[A, B](v: String, `type`: Type[A], body: Term[B]) extends Term[A => B]
//    case class App[A, B](f: Term[A => B], a: Term[A]) extends Term[A]
//    case class Lit(n: Int) extends Term[Int]
//    case class Add(a: Term[Int], b: Term[Int]) extends Term[Int]
//
//    case class Typed[T](`type`: Type[T], term: Term[T])
//    case class ExTyped(typed: Typed[_])
//
//    case class ExType(`type`: Type[_])
//  }
//
//  def typeCheckType(u: Untyped.Type): Typed.ExType =
//    u match {
//      case Untyped.Integer => Typed.ExType(Typed.Integer)
//      case Untyped.Arrow(in, out) =>
//        (typeCheckType(in), typeCheckType(out)) match {
//          case (ExType(tIn), ExType(tOut)) =>
//            Typed.ExType(Typed.Arrow(tIn, tOut))
//        }
//    }
//
//  def typeCheckTerm(u: Untyped.Term): Typed.Typed[_] =
//    u match {
//      case Untyped.Lam(v, t, body) =>
//        (typeCheckType(t), typeCheckTerm(body)) match {
//          case (ExType(varType), Typed.Typed(typeBody, bodyTyped)) =>
//            Typed.Typed(
//              Typed.Arrow(varType, typeBody),
//              Typed.Lam(v, varType, bodyTyped)
//            )
//        }
//      case Untyped.App(f, in) =>
//        typeCheckTerm(f) match {
//          case Typed.Typed(Typed.Arrow(fInType, fOutType), typedF) =>
//            typeCheckTerm(in) match {
//              case Typed.Typed(inType, typedIn) if fInType == inType =>
//                Typed.Typed(fOutType, Typed.App(typedF, typedIn))
//            }
//
//        }
//      case Untyped.Lit(n)    => ???
//      case Untyped.Add(a, b) => ???
//    }
//
//}
