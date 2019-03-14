package evolution.language
import cats.{ Applicative, Eq, Group }
import evolution.geometry
import evolution.geometry.Point
import evolution.typeclass.VectorSpace
import cats.implicits._
import cats.kernel.Order
import cats.mtl.FunctorRaise

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

    def domain[M[_]](t: Type)(implicit A: Applicative[M], E: FunctorRaise[M, String]): M[Type] = t match {
      case Type.Arrow(from, to) => from.pure[M]
      case _                    => E.raise(s"Type $t is not an Arrow type")
    }

    def codomain[M[_]](t: Type)(implicit A: Applicative[M], E: FunctorRaise[M, String]): M[Type] = t match {
      case Type.Arrow(from, to) => to.pure[M]
      case _                    => E.raise(s"Type $t is not an Arrow type")
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
    type Instance = Qualified[Predicate]
    object Instance {
      def apply(predicates: List[Predicate], predicate: Predicate): Instance = Qualified(predicates, predicate)
    }
    case class ClassDef(instances: List[Instance])

    // example: Int is Ord, Dbl is Ord, Dbl Ord => Point Ord
    ClassDef(
      List(
        Instance(Nil, Predicate("Ord", List(Type.Integer))),
        Instance(Nil, Predicate("Ord", List(Type.Dbl))),
        Instance(List(Predicate("Ord", List(Type.Dbl))), Predicate("Ord", List(Type.Point)))
      ))
  }
}
