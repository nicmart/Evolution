package evolution.compiler.types

import cats.implicits._
import cats.{ Eq, Group, Order }
import evolution.geometry.Point
import evolution.materialization.Evolution
import evolution.typeclass.Semigroupoid
import evolution.typeclass.Semigroupoid._
import evolution.typeclass.Invertible

sealed trait Type {
  type Out

  final def children: List[Type] = this match {
    case Type.Evo(inner)      => List(inner)
    case Type.Lst(inner)      => List(inner)
    case Type.Arrow(from, to) => List(from, to)
    case _                    => Nil
  }

  final def =>:(from: Type): Type = Type.Arrow(from, this)

  final def typeVars: Set[Type.Var] = this match {
    case Type.Var(name) => Set(Type.Var(name))
    case _              => children.flatMap(_.typeVars).toSet
  }

  final def typeVarUsages(varName: String): List[Type] =
    typeVars.collect {
      case tpe @ Type.Var(name) if varName == name => tpe
    }.toList
}

object Type {
  final case class Var(name: String) extends Type {
    type Out = Nothing
    override def toString: String = name
  }

  final case object Integer extends Type { type Out = Int }
  final case object Dbl extends Type { type Out = Double }
  final case object Point extends Type { type Out = Point }
  final case object Bool extends Type { type Out = Boolean }
  final case class Evo(inner: Type) extends Type { type Out = Evolution[inner.type] }
  final case class Lst(inner: Type) extends Type { type Out = List[inner.type] }
  final case class Arrow(from: Type, to: Type) extends Type {
    type Out = from.type => to.type
    override def toString: String = s"$from -> $to"
  }

  // TODO can we do better thant this?
  def group(t: Type): Either[String, Group[t.Out]] = {
    t match {
      case Type.Integer => Group[Int].asRight
      case Type.Dbl     => Group[Double].asRight
      case Type.Point   => Group[Point].asRight
      case _            => s"Unable to find a group for type $t".asLeft
    }
  }.map(_.innerAs[t.Out])

  def semigroup(
    t: Type
  ): Either[String, Semigroupoid[t.Out, t.Out, t.Out]] =
    addSemigrupoid(t, t, t)

  def multSemigrupoid(t1: Type, t2: Type, t3: Type): Either[String, Semigroupoid[t1.Out, t2.Out, t3.Out]] = {
    (t1, t2, t3) match {
      case (Type.Dbl, Type.Dbl, Type.Dbl)                         => Multiplicative.dblDblDbl.asRight
      case (Type.Dbl, Type.Point, Type.Point)                     => Multiplicative.dblPointPoint.asRight
      case (Type.Point, Type.Dbl, Type.Point)                     => Multiplicative.pointDblPoint.asRight
      case (Type.Integer, Type.Integer, Type.Integer)             => Multiplicative.intIntInt.asRight
      case (Type.Integer, Type.Dbl, Type.Dbl)                     => Multiplicative.intDblDbl.asRight
      case (Type.Dbl, Type.Integer, Type.Dbl)                     => Multiplicative.dblIntDbl.asRight
      case (Type.Integer, Type.Point, Type.Point)                 => Multiplicative.intPointPoint.asRight
      case (Type.Dbl, Type.Evo(Type.Dbl), Type.Evo(Type.Dbl))     => Multiplicative.dblEvoDblEvoDbl.asRight
      case (Type.Evo(Type.Dbl), Type.Dbl, Type.Evo(Type.Dbl))     => Multiplicative.evoDblDblEvoDbl.asRight
      case (Type.Dbl, Type.Evo(Type.Point), Type.Evo(Type.Point)) => Multiplicative.dblEvoPointEvoPoint.asRight
      case (Type.Evo(Type.Point), Type.Dbl, Type.Evo(Type.Point)) => Multiplicative.evoPointDblEvoPoint.asRight
      case (Type.Evo(Type.Dbl), Type.Evo(Type.Dbl), Type.Evo(Type.Dbl)) =>
        Multiplicative.evoDblEvoDblEvoDbl.asRight
      case (Type.Evo(Type.Point), Type.Evo(Type.Dbl), Type.Evo(Type.Point)) =>
        Multiplicative.evoPointEvoDblEvoPoint.asRight
      case (Type.Evo(Type.Dbl), Type.Evo(Type.Point), Type.Evo(Type.Point)) =>
        Multiplicative.evoDblEvoPointEvoPoint.asRight
      case _ => s"Unable to find a Mult instance for types $t1, $t2, $t3".asLeft
    }
  }.map(_.innerAs[t1.Out, t2.Out, t3.Out])

  def addSemigrupoid(t1: Type, t2: Type, t3: Type): Either[String, Semigroupoid[t1.Out, t2.Out, t3.Out]] = {
    (t1, t2, t3) match {
      case (Type.Dbl, Type.Dbl, Type.Dbl)             => Additive.dblDblDbl.asRight
      case (Type.Integer, Type.Integer, Type.Integer) => Additive.intIntInt.asRight
      case (Type.Integer, Type.Dbl, Type.Dbl)         => Additive.intDblDbl.asRight
      case (Type.Dbl, Type.Integer, Type.Dbl)         => Additive.dblIntDbl.asRight
      case (Type.Point, Type.Point, Type.Point)       => Additive.pointPointPoint.asRight
      case (Type.Evo(Type.Point), Type.Evo(Type.Point), Type.Evo(Type.Point)) =>
        Additive.evoPointEvoPointEvoPoint.asRight
      case (Type.Evo(Type.Dbl), Type.Evo(Type.Dbl), Type.Evo(Type.Dbl)) => Additive.evoDblEvoDblEvoDbl.asRight
      case _                                                            => s"Unable to find an Add instance for types $t1, $t2, $t3".asLeft
    }
  }.map(_.innerAs[t1.Out, t2.Out, t3.Out])

  def eqTypeClass(t: Type): Either[String, Eq[t.Out]] = {
    t match {
      case Type.Integer => Eq[Int].asRight
      case Type.Dbl     => Eq[Double].asRight
      case Type.Point   => Eq[Point].asRight
      case Type.Bool    => Eq[Boolean].asRight
      case _            => s"Unable to find an eq typeclass for type $t".asLeft
    }
  }.map(_.innerAs[t.Out])

  def invertible(t: Type): Either[String, Invertible[t.Out]] = {
    t match {
      case Type.Integer         => Invertible.Additive.intInvertible.asRight
      case Type.Dbl             => Invertible.Additive.dblInvertible.asRight
      case Type.Point           => Invertible.Additive.pointInvertible.asRight
      case Type.Evo(Type.Dbl)   => Invertible.Additive.dblEvoInvertible.asRight
      case Type.Evo(Type.Point) => Invertible.Additive.pointEvoInvertible.asRight
      case _                    => s"Unable to find an invertible typeclass for type $t".asLeft
    }
  }.map(_.innerAs[t.Out])

  def invertibleSemigroup(t: Type): Either[String, (Semigroup[t.Out], Invertible[t.Out])] =
    (semigroup(t), invertible(t)).tupled

  def order(t: Type): Either[String, Order[t.Out]] = {
    t match {
      case Type.Integer => Order[Int].asRight
      case Type.Dbl     => Order[Double].asRight
      case _            => s"Unable to find an eq typeclass for type $t".asLeft
    }
  }.map(_.innerAs[t.Out])

  // TODO Bleah, I would like the AST to be hosting the inner type
  def unwrapEvo(t: Type): Either[String, Type] = t match {
    case Type.Evo(inner) => inner.asRight
    case _               => s"Type $t is not an Evolution type".asLeft
  }

  def unwrapLst(t: Type): Either[String, Type] = t match {
    case Type.Lst(inner) => inner.asRight
    case _               => s"Type $t is not a Lst type".asLeft
  }

  private implicit class Casts1[F[_], T](f: F[T]) {
    def innerAs[S]: F[S] = f.asInstanceOf[F[S]]
  }

  private implicit class Casts3[F[_, _, _], T1, T2, T3](f: F[T1, T2, T3]) {
    def innerAs[S1, S2, S3]: F[S1, S2, S3] = f.asInstanceOf[F[S1, S2, S3]]
  }
}
