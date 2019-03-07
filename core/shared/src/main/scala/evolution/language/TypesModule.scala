package evolution.language
import cats.{ Eq, Group }
import evolution.geometry
import evolution.geometry.Point
import evolution.typeclass.VectorSpace
import cats.implicits._
import cats.kernel.Order

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
    def group(t: Type): Either[String, Group[t.Out]] = {
      t match {
        case Type.Integer => Right(Group[Int])
        case Type.Dbl     => Right(Group[Double])
        case Type.Point   => Right(Group[Point])
        case _            => Left(s"Unable to find a group for type $t")
      }
    }.asInstanceOf[Either[String, Group[t.Out]]]

    def vectorSpace(t: Type): Either[String, VectorSpace[t.Out]] = {
      t match {
        case Type.Integer => Right(VectorSpace[Int])
        case Type.Dbl     => Right(VectorSpace[Double])
        case Type.Point   => Right(VectorSpace[Point])
        case _            => Left(s"Unable to find a vector space for type $t")
      }
    }.asInstanceOf[Either[String, VectorSpace[t.Out]]]

    def eqTypeClass(t: Type): Either[String, Eq[t.Out]] = {
      t match {
        case Type.Integer => Right(Eq[Int])
        case Type.Dbl     => Right(Eq[Double])
        case Type.Point   => Right(Eq[Point])
        case Type.Bool    => Right(Eq[Boolean])
        case _            => Left(s"Unable to find an eq typeclass for type $t")
      }
    }.asInstanceOf[Either[String, Eq[t.Out]]]

    def order(t: Type): Either[String, Order[t.Out]] = {
      t match {
        case Type.Integer => Right(Order[Int])
        case Type.Dbl     => Right(Order[Double])
        case _            => Left(s"Unable to find an eq typeclass for type $t")
      }
    }.asInstanceOf[Either[String, Order[t.Out]]]

    def unwrapF(t: Type): Either[String, Type] = t match {
      case Type.Evo(inner) => Right(inner)
      case _               => Left(s"Type $t is not an Evolution type")
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

  object TypeClasses {
    case class Predicate(id: String, types: List[Type])
    case class Qualified[T](predicates: List[Predicate], t: T)
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
