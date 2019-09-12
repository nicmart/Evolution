package evolution.compiler.types

import cats.implicits._
import evolution.geometry.Point
import evolution.materialization.Evolution
import evolution.compiler.expression.typeclass._

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
  final case object Double extends Type { type Out = Double }
  final case object Point extends Type { type Out = Point }
  final case object Bool extends Type { type Out = Boolean }
  final case class Evo(inner: Type) extends Type { type Out = Evolution[inner.type] }
  final case class Lst(inner: Type) extends Type { type Out = List[inner.type] }
  final case class Arrow(from: Type, to: Type) extends Type {
    type Out = from.type => to.type
    override def toString: String = s"$from -> $to"
  }

  def semigroup(
    t: Type
  ): Either[String, Additive[t.Out, t.Out, t.Out]] =
    addSemigrupoid(t, t, t)

  def multSemigrupoid(t1: Type, t2: Type, t3: Type): Either[String, Multiplicative[t1.Out, t2.Out, t3.Out]] = {
    (t1, t2, t3) match {
      case (Type.Double, Type.Double, Type.Double)    => Multiplicative.DoubleDoubleDouble.asRight
      case (Type.Double, Type.Point, Type.Point)      => Multiplicative.DoublePointPoint.asRight
      case (Type.Point, Type.Double, Type.Point)      => Multiplicative.PointDoublePoint.asRight
      case (Type.Integer, Type.Integer, Type.Integer) => Multiplicative.IntIntInt.asRight
      case (Type.Integer, Type.Double, Type.Double)   => Multiplicative.IntDoubleDouble.asRight
      case (Type.Double, Type.Integer, Type.Double)   => Multiplicative.DoubleIntDouble.asRight
      case (Type.Integer, Type.Point, Type.Point)     => Multiplicative.IntPointPoint.asRight
      case (Type.Double, Type.Evo(Type.Double), Type.Evo(Type.Double)) =>
        Multiplicative.DoubleEvoDoubleEvoDouble.asRight
      case (Type.Evo(Type.Double), Type.Double, Type.Evo(Type.Double)) =>
        Multiplicative.EvoDoubleDoubleEvoDouble.asRight
      case (Type.Double, Type.Evo(Type.Point), Type.Evo(Type.Point)) => Multiplicative.DoubleEvoPointEvoPoint.asRight
      case (Type.Evo(Type.Point), Type.Double, Type.Evo(Type.Point)) => Multiplicative.EvoPointDoubleEvoPoint.asRight
      case (Type.Evo(Type.Double), Type.Evo(Type.Double), Type.Evo(Type.Double)) =>
        Multiplicative.EvoDoubleEvoDoubleEvoDouble.asRight
      case (Type.Evo(Type.Point), Type.Evo(Type.Double), Type.Evo(Type.Point)) =>
        Multiplicative.EvoPointEvoDoubleEvoPoint.asRight
      case (Type.Evo(Type.Double), Type.Evo(Type.Point), Type.Evo(Type.Point)) =>
        Multiplicative.EvoDoubleEvoPointEvoPoint.asRight
      case _ => s"Unable to find a Mult instance for types $t1, $t2, $t3".asLeft
    }
  }.map(_.asInstanceOf[Multiplicative[_, _, _]].innerAs[t1.Out, t2.Out, t3.Out])

  def addSemigrupoid(t1: Type, t2: Type, t3: Type): Either[String, Additive[t1.Out, t2.Out, t3.Out]] = {
    (t1, t2, t3) match {
      case (Type.Double, Type.Double, Type.Double)    => Additive.DoubleDoubleDouble.asRight
      case (Type.Integer, Type.Integer, Type.Integer) => Additive.IntIntInt.asRight
      case (Type.Integer, Type.Double, Type.Double)   => Additive.IntDoubleDouble.asRight
      case (Type.Double, Type.Integer, Type.Double)   => Additive.DoubleIntDouble.asRight
      case (Type.Point, Type.Point, Type.Point)       => Additive.PointPointPoint.asRight
      case (Type.Evo(Type.Point), Type.Evo(Type.Point), Type.Evo(Type.Point)) =>
        Additive.EvoPointEvoPointEvoPoint.asRight
      case (Type.Evo(Type.Double), Type.Evo(Type.Double), Type.Evo(Type.Double)) =>
        Additive.EvoDoubleEvoDoubleEvoDouble.asRight
      case _ => s"Unable to find an Add instance for types $t1, $t2, $t3".asLeft
    }
  }.map(_.asInstanceOf[Additive[_, _, _]].innerAs[t1.Out, t2.Out, t3.Out])

  def eqTypeClass(t: Type): Either[String, Equable[t.Out]] = {
    t match {
      case Type.Integer => Equable.Int.asRight
      case Type.Double  => Equable.Double.asRight
      case Type.Point   => Equable.Point.asRight
      case Type.Bool    => Equable.Boolean.asRight
      case _            => s"Unable to find an eq typeclass for type $t".asLeft
    }
  }.map(_.asInstanceOf[Equable[_]].innerAs[t.Out])

  def invertible(t: Type): Either[String, Invertible[t.Out]] = {
    t match {
      case Type.Integer          => Invertible.Int.asRight
      case Type.Double           => Invertible.Double.asRight
      case Type.Point            => Invertible.Point.asRight
      case Type.Evo(Type.Double) => Invertible.DoubleEvo.asRight
      case Type.Evo(Type.Point)  => Invertible.PointEvo.asRight
      case _                     => s"Unable to find an invertible typeclass for type $t".asLeft
    }
  }.map(_.asInstanceOf[Invertible[_]].innerAs[t.Out])

  def invertibleSemigroup(t: Type): Either[String, (Additive[t.Out, t.Out, t.Out], Invertible[t.Out])] =
    (semigroup(t), invertible(t)).tupled

  def order(t: Type): Either[String, Comparable[t.Out]] = {
    t match {
      case Type.Integer => Comparable.Int.asRight
      case Type.Double  => Comparable.Double.asRight
      case _            => s"Unable to find an eq typeclass for type $t".asLeft
    }
  }.map(_.asInstanceOf[Comparable[_]].innerAs[t.Out])

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
