package evolution.language
import cats.{ Applicative, Eq, Group }
import evolution.geometry
import evolution.geometry.Point
import evolution.typeclass.{ LeftModule, VectorSpace }
import cats.implicits._
import cats.kernel.Order
import cats.mtl.FunctorRaise
import evolution.materialization.Iterable
import evolution.typeclass.Semigroupoid
import evolution.typeclass.Semigroupoid._

trait TypesModule[F[_]] {

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
    final case class Evo(inner: Type) extends Type { type Out = F[inner.type] }
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

    def vectorSpace[M[_]](t: Type)(implicit A: Applicative[M], E: FunctorRaise[M, String]): M[VectorSpace[t.Out]] = {
      t match {
        case Type.Integer => VectorSpace[Int].pure[M]
        case Type.Dbl     => VectorSpace[Double].pure[M]
        case Type.Point   => VectorSpace[Point].pure[M]
        case _            => E.raise(s"Unable to find a vector space for type $t")
      }
    }.asInstanceOf[M[VectorSpace[t.Out]]]

    def leftModule[M[_]](
      t1: Type,
      t2: Type
    )(implicit A: Applicative[M], E: FunctorRaise[M, String]): M[LeftModule[t1.Out, t2.Out]] = {
      (t1, t2) match {
        case (Type.Dbl, Type.Dbl)   => LeftModule[Double, Double].pure[M]
        case (Type.Dbl, Type.Point) => LeftModule[Double, Point].pure[M]
        case (Type.Dbl, Type.Evo(Type.Point)) =>
          LeftModule[Double, Iterable[Point]].pure[M] // TODO Ouch, here we have to use the concrete F!
        case (Type.Integer, Type.Integer) => LeftModule[Int, Int].pure[M]
        case (Type.Integer, Type.Dbl)     => LeftModule[Int, Double].pure[M]
        case (Type.Integer, Type.Point)   => LeftModule[Int, Point].pure[M]
        case _                            => E.raise(s"Unable to find a LeftModule instance for types $t1, $t2")
      }
    }.asInstanceOf[M[LeftModule[t1.Out, t2.Out]]]

    def multSemigrupoid[M[_]](t1: Type, t2: Type, t3: Type)(implicit A: Applicative[M], E: FunctorRaise[M, String]): M[Semigroupoid[t1.Out, t2.Out, t3.Out]] =
      {(t1, t2, t3) match {
        case (Type.Dbl, Type.Dbl, Type.Dbl) => Multiplicative.dblDblDbl.pure[M]
        case (Type.Dbl, Type.Point, Type.Point) => Multiplicative.dblPointPoint.pure[M]
        case (Type.Point, Type.Dbl, Type.Point) => Multiplicative.pointDblPoint.pure[M]
        case (Type.Integer, Type.Integer, Type.Integer) => Multiplicative.intIntInt.pure[M]
        case (Type.Integer, Type.Dbl, Type.Dbl) => Multiplicative.intDblDbl.pure[M]
        case (Type.Dbl, Type.Integer, Type.Dbl) => Multiplicative.dblIntDbl.pure[M]
        case (Type.Integer, Type.Point, Type.Point) => Multiplicative.intPointPoint.pure[M]
        case (Type.Dbl, Type.Evo(Type.Dbl), Type.Evo(Type.Dbl)) => Multiplicative.dblEvoDblEvoDbl.pure[M]
        case (Type.Evo(Type.Dbl), Type.Dbl, Type.Evo(Type.Dbl)) => Multiplicative.evoDblDblEvoDbl.pure[M]
        case (Type.Dbl, Type.Evo(Type.Point), Type.Evo(Type.Point)) => Multiplicative.dblEvoPointEvoPoint.pure[M]
        case (Type.Evo(Type.Point), Type.Dbl, Type.Evo(Type.Point)) => Multiplicative.evoPointDblEvoPoint.pure[M]
        case _                            => E.raise(s"Unable to find a Mult instance for types $t1, $t2, $t3")
      }}.asInstanceOf[M[Semigroupoid[t1.Out, t2.Out, t3.Out]]]

    def eqTypeClass[M[_]](t: Type)(implicit A: Applicative[M], E: FunctorRaise[M, String]): M[Eq[t.Out]] = {
      t match {
        case Type.Integer => Eq[Int].pure[M]
        case Type.Dbl     => Eq[Double].pure[M]
        case Type.Point   => Eq[Point].pure[M]
        case Type.Bool    => Eq[Boolean].pure[M]
        case _            => E.raise(s"Unable to find an eq typeclass for type $t")
      }
    }.asInstanceOf[M[Eq[t.Out]]]

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

  def lift(tpe: Type): Type = tpe match {
    case Type.Arrow(from, to) => lift(from) =>: lift(to)
    case _                    => Type.Evo(tpe)
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
}
