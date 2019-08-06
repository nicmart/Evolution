package evolution.language
import cats.{ Applicative, Eq, Group }
import evolution.geometry
import evolution.geometry.Point
import cats.implicits._
import cats.kernel.Order
import cats.mtl.FunctorRaise
import evolution.typeclass.Semigroupoid
import evolution.typeclass.Semigroupoid._
import evolution.typeclass.Invertible
import evolution.materialization.Evolution

sealed trait Type {
  type Out
  def children: List[Type] = this match {
    case Type.Evo(inner)      => List(inner)
    case Type.Lst(inner)      => List(inner)
    case Type.Arrow(from, to) => List(from, to)
    case _                    => Nil
  }

  def =>:(from: Type): Type = Type.Arrow(from, this)
}

object Type {
  final case class Var(name: String) extends Type {
    type Out = Nothing
    override def toString: String = name
  }

  final case object Integer extends Type { type Out = Int }
  final case object Dbl extends Type { type Out = Double }
  final case object Point extends Type { type Out = geometry.Point }
  final case object Bool extends Type { type Out = Boolean }
  final case class Evo(inner: Type) extends Type { type Out = Evolution[inner.type] }
  final case class Lst(inner: Type) extends Type { type Out = List[inner.type] }
  final case class Arrow(from: Type, to: Type) extends Type { type Out = from.type => to.type }

  // TODO can we do better thant this?
  def group[M[_]](t: Type)(implicit A: Applicative[M], E: FunctorRaise[M, String]): M[Group[t.Out]] = {
    t match {
      case Type.Integer => Group[Int].pure[M]
      case Type.Dbl     => Group[Double].pure[M]
      case Type.Point   => Group[Point].pure[M]
      case _            => E.raise(s"Unable to find a group for type $t")
    }
  }.asInstanceOf[M[Group[t.Out]]]

  def semigroup[M[_]](
    t: Type
  )(implicit A: Applicative[M], E: FunctorRaise[M, String]): M[Semigroupoid[t.Out, t.Out, t.Out]] =
    addSemigrupoid[M](t, t, t)

  def multSemigrupoid[M[_]](t1: Type, t2: Type, t3: Type)(
    implicit A: Applicative[M],
    E: FunctorRaise[M, String]
  ): M[Semigroupoid[t1.Out, t2.Out, t3.Out]] = {
    import Multiplicative._
    (t1, t2, t3) match {
      case (Type.Dbl, Type.Dbl, Type.Dbl)                                   => dblDblDbl.pure[M]
      case (Type.Dbl, Type.Point, Type.Point)                               => dblPointPoint.pure[M]
      case (Type.Point, Type.Dbl, Type.Point)                               => pointDblPoint.pure[M]
      case (Type.Integer, Type.Integer, Type.Integer)                       => intIntInt.pure[M]
      case (Type.Integer, Type.Dbl, Type.Dbl)                               => intDblDbl.pure[M]
      case (Type.Dbl, Type.Integer, Type.Dbl)                               => dblIntDbl.pure[M]
      case (Type.Integer, Type.Point, Type.Point)                           => intPointPoint.pure[M]
      case (Type.Dbl, Type.Evo(Type.Dbl), Type.Evo(Type.Dbl))               => dblEvoDblEvoDbl.pure[M]
      case (Type.Evo(Type.Dbl), Type.Dbl, Type.Evo(Type.Dbl))               => evoDblDblEvoDbl.pure[M]
      case (Type.Dbl, Type.Evo(Type.Point), Type.Evo(Type.Point))           => dblEvoPointEvoPoint.pure[M]
      case (Type.Evo(Type.Point), Type.Dbl, Type.Evo(Type.Point))           => evoPointDblEvoPoint.pure[M]
      case (Type.Evo(Type.Dbl), Type.Evo(Type.Dbl), Type.Evo(Type.Dbl))     => evoDblEvoDblEvoDbl.pure[M]
      case (Type.Evo(Type.Point), Type.Evo(Type.Dbl), Type.Evo(Type.Point)) => evoPointEvoDblEvoPoint.pure[M]
      case (Type.Evo(Type.Dbl), Type.Evo(Type.Point), Type.Evo(Type.Point)) => evoDblEvoPointEvoPoint.pure[M]
      case _                                                                => E.raise(s"Unable to find a Mult instance for types $t1, $t2, $t3")
    }
  }.asInstanceOf[M[Semigroupoid[t1.Out, t2.Out, t3.Out]]]

  def addSemigrupoid[M[_]](t1: Type, t2: Type, t3: Type)(
    implicit A: Applicative[M],
    E: FunctorRaise[M, String]
  ): M[Semigroupoid[t1.Out, t2.Out, t3.Out]] = {
    (t1, t2, t3) match {
      case (Type.Dbl, Type.Dbl, Type.Dbl)             => Additive.dblDblDbl.pure[M]
      case (Type.Integer, Type.Integer, Type.Integer) => Additive.intIntInt.pure[M]
      case (Type.Integer, Type.Dbl, Type.Dbl)         => Additive.intDblDbl.pure[M]
      case (Type.Dbl, Type.Integer, Type.Dbl)         => Additive.dblIntDbl.pure[M]
      case (Type.Point, Type.Point, Type.Point)       => Additive.pointPointPoint.pure[M]
      case (Type.Evo(Type.Point), Type.Evo(Type.Point), Type.Evo(Type.Point)) =>
        Additive.evoPointEvoPointEvoPoint.pure[M]
      case (Type.Evo(Type.Dbl), Type.Evo(Type.Dbl), Type.Evo(Type.Dbl)) => Additive.evoDblEvoDblEvoDbl.pure[M]
      case _                                                            => E.raise(s"Unable to find an Add instance for types $t1, $t2, $t3")
    }
  }.asInstanceOf[M[Semigroupoid[t1.Out, t2.Out, t3.Out]]]

  def eqTypeClass[M[_]](t: Type)(implicit A: Applicative[M], E: FunctorRaise[M, String]): M[Eq[t.Out]] = {
    t match {
      case Type.Integer => Eq[Int].pure[M]
      case Type.Dbl     => Eq[Double].pure[M]
      case Type.Point   => Eq[Point].pure[M]
      case Type.Bool    => Eq[Boolean].pure[M]
      case _            => E.raise(s"Unable to find an eq typeclass for type $t")
    }
  }.asInstanceOf[M[Eq[t.Out]]]

  def invertible[M[_]](t: Type)(implicit A: Applicative[M], E: FunctorRaise[M, String]): M[Invertible[t.Out]] = {
    t match {
      case Type.Integer         => Invertible.Additive.intInvertible.pure[M]
      case Type.Dbl             => Invertible.Additive.dblInvertible.pure[M]
      case Type.Point           => Invertible.Additive.pointInvertible.pure[M]
      case Type.Evo(Type.Dbl)   => Invertible.Additive.dblEvoInvertible.pure[M]
      case Type.Evo(Type.Point) => Invertible.Additive.pointEvoInvertible.pure[M]
      case _                    => E.raise(s"Unable to find an invertible typeclass for type $t")
    }
  }.asInstanceOf[M[Invertible[t.Out]]]

  def invertibleSemigroup[M[_]](
    t: Type
  )(implicit A: Applicative[M], E: FunctorRaise[M, String]): M[(Semigroup[t.Out], Invertible[t.Out])] =
    A.tuple2(semigroup[M](t), invertible[M](t))

  def order[M[_]](t: Type)(implicit A: Applicative[M], E: FunctorRaise[M, String]): M[Order[t.Out]] = {
    t match {
      case Type.Integer => Order[Int].pure[M]
      case Type.Dbl     => Order[Double].pure[M]
      case _            => E.raise(s"Unable to find an eq typeclass for type $t")
    }
  }.asInstanceOf[M[Order[t.Out]]]

  def unwrapF[M[_]](t: Type)(implicit A: Applicative[M], E: FunctorRaise[M, String]): M[Type] = t match {
    case Type.Evo(inner) => inner.pure[M]
    case _               => E.raise(s"Type $t is not an Evolution type")
  }
}

final class Context(bindings: Map[String, Type]) {
  def has(name: String): Boolean = bindings.isDefinedAt(name)
  def get(name: String): Option[Type] = bindings.get(name)
  def put(name: String, tpe: Type): Context = new Context(bindings.updated(name, tpe))
  def nextVar: String = "X" + bindings.size
  def nextTypeVar: Type = Type.Var(nextVar)
}

object Context {
  val empty: Context = new Context(Map.empty)
}

case class Typed[T](tpe: Type, value: T)

// Not everything defined here is used yet, but let's keep it, since it is a valid modeling
// of the data types in the paper "Typing Haskell in Haskell"
object TypeClasses {
  case class Predicate(id: String, types: List[Type])
  case class Qualified[T](predicates: List[Predicate], t: T)
  object Qualified {
    def apply[T](t: T): Qualified[T] = Qualified(Nil, t)
  }
  case class Default(id: String, tpe: Type)
}