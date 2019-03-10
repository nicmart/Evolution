package evolution.language
import cats.MonadError
import enumeratum.{ Enum, EnumEntry }
import enumeratum.EnumEntry.Lowercase
import evolution.data.ExpressionModule
import cats.implicits._

import scala.collection.immutable

trait PredefinedConstantsModule[F[_]] { self: TypesModule[F] with ExpressionModule[F] =>
  import TypeClasses._, Type._

  abstract sealed class Constant(val qualifiedType: Qualified[Type]) extends EnumEntry with Lowercase

  abstract sealed class Constant0(qualifiedType: Qualified[Type])
      extends Constant(qualifiedType)
      with EnumEntry
      with Lowercase {

    def compile[M[_]](implicit M: MonadError[M, String]): M[Expr[_]]
  }

  object Constant0 extends Enum[Constant0] {
    val values: immutable.IndexedSeq[Constant0] = findValues

    case object PI extends Constant0(Qualified(Dbl)) {
      def compile[M[_]](implicit M: MonadError[M, String]): M[Expr[_]] =
        Expr.Dbl(Math.PI).pure[M].widen
    }

    case object Empty extends Constant0(Qualified(Var("T"))) {
      def compile[M[_]](implicit M: MonadError[M, String]): M[Expr[_]] =
        Expr.Empty().pure[M].widen
    }

    def unapply(s: String): Option[Constant0] = withNameInsensitiveOption(s)
  }

  abstract sealed class Constant1(qualifiedType: Qualified[Type])
      extends Constant(qualifiedType)
      with EnumEntry
      with Lowercase

  object Constant1 extends Enum[Constant1] {
    val values: immutable.IndexedSeq[Constant1] = findValues

    case object X extends Constant1(Qualified(Type.Point =>: Dbl))
    case object Y extends Constant1(Qualified(Type.Point =>: Dbl))
    case object Floor extends Constant1(Qualified(Dbl =>: Integer))
    case object ToDbl extends Constant1(Qualified(Integer =>: Dbl))
    case object Abs extends Constant1(Qualified(Dbl =>: Dbl))
    case object Sign extends Constant1(Qualified(Dbl =>: Dbl))
    case object Inverse extends Constant1(Qualified(Var("T") =>: Var("T")))
    case object Sin extends Constant1(Qualified(Dbl =>: Dbl))
    case object Cos extends Constant1(Qualified(Dbl =>: Dbl))
    case object Not extends Constant1(Qualified(Bool =>: Bool))
    case object Lift extends Constant1(Qualified(Var("T")))
    case object Fix extends Constant1(Qualified((Var("T") =>: Var("T")) =>: Var("T")))
    case object Constant extends Constant1(Qualified(Var("T") =>: Evo(Var("T"))))

    def unapply(s: String): Option[Constant1] = withNameInsensitiveOption(s)
  }

  abstract sealed class Constant2(qualifiedType: Qualified[Type])
      extends Constant(qualifiedType)
      with EnumEntry
      with Lowercase

  object Constant2 extends Enum[Constant2] {
    val values: immutable.IndexedSeq[Constant2] = findValues

    case object Point extends Constant2(Qualified(Dbl =>: Dbl =>: Type.Point))
    case object Polar extends Constant2(Qualified(Dbl =>: Dbl =>: Type.Point))
    case object Multiply extends Constant2(Qualified(Dbl =>: Var("T") =>: Var("T")))
    case object Add
        extends Constant2(Qualified(List(Predicate("Semigroup", List(Var("T")))), Var("T") =>: Var("T") =>: Var("T")))
    case object Minus
        extends Constant2(Qualified(List(Predicate("Semigroup", List(Var("T")))), Var("T") =>: Var("T") =>: Var("T")))
    case object Div extends Constant2(Qualified(Dbl =>: Dbl =>: Dbl))
    case object Exp extends Constant2(Qualified(Dbl =>: Dbl =>: Dbl))
    case object Mod extends Constant2(Qualified(Dbl =>: Dbl =>: Dbl))
    case object And extends Constant2(Qualified(Bool =>: Bool =>: Bool))
    case object Or extends Constant2(Qualified(Bool =>: Bool =>: Bool))
    case object Eq extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool))
    case object Neq extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool))
    case object GreaterThan extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool))
    case object GreaterThanOrEqual extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool))
    case object LessThan extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool))
    case object LessThanOrEqual extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool))
    case object Cons extends Constant2(Qualified(Var("T") =>: Evo(Var("T")) =>: Evo(Var("T"))))
    case object MapEmpty extends Constant2(Qualified(Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T"))))
    case object MapCons
        extends Constant2(
          Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T1")) =>: Evo(Var("T2"))) =>: Evo(Var("T2"))))

    case object Integrate extends Constant2(Qualified(Var("T") =>: Evo(Var("T")) =>: Evo(Var("T"))))
    case object Solve1 extends Constant2(Qualified(Evo(Var("T") =>: Var("T")) =>: Var("T") =>: Evo(Var("T"))))
    case object Concat extends Constant2(Qualified(Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T"))))
    case object Map extends Constant2(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Var("T2")) =>: Evo(Var("T2"))))
    case object FlatMap
        extends Constant2(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2"))))
    case object Take extends Constant2(Qualified(Integer =>: Evo(Var("T")) =>: Evo(Var("T"))))
    case object While extends Constant2(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Bool) =>: Evo(Var("T1"))))
    case object Until extends Constant2(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Bool) =>: Evo(Var("T1"))))
    case object Uniform extends Constant2(Qualified(Dbl =>: Dbl =>: Evo(Dbl)))
    case object UniformFrom extends Constant2(Qualified(Integer =>: Evo(Var("T")) =>: Evo(Var("T"))))
    case object Normal extends Constant2(Qualified(Dbl =>: Dbl =>: Evo(Dbl)))

    def unapply(s: String): Option[Constant2] = withNameInsensitiveOption(s)
  }

  abstract sealed class Constant3(qualifiedType: Qualified[Type])
      extends Constant(qualifiedType)
      with EnumEntry
      with Lowercase

  object Constant3 extends Enum[Constant3] {
    val values: immutable.IndexedSeq[Constant3] = findValues

    case object ZipWith
        extends Constant3(
          Qualified(Evo(Var("T1")) =>: Evo(Var("T2")) =>: (Var("T1") =>: Var("T2") =>: Var("T3")) =>: Evo(Var("T3"))))
    case object Solve2
        extends Constant3(
          Qualified(Evo(Var("T") =>: Var("T") =>: Var("T")) =>: Var("T") =>: Var("T") =>: Evo(Var("T"))))

    case object InRect extends Constant3(Qualified(Type.Point =>: Type.Point =>: Type.Point =>: Bool))
    case object If extends Constant3(Qualified(Bool =>: Var("T") =>: Var("T") =>: Var("T")))
    case object UniformDiscrete extends Constant3(Qualified(Dbl =>: Dbl =>: Dbl =>: Evo(Dbl)))

    def unapply(s: String): Option[Constant3] = withNameInsensitiveOption(s)
  }

  object Constant {
    val values
      : immutable.IndexedSeq[Constant] = Constant0.values ++ Constant1.values ++ Constant2.values ++ Constant3.values
    val functions0: List[Constant0] = List(Constant0.Empty, Constant0.PI)
    val nonFunctions0: List[Constant] = values.toList.filter(!functions0.contains(_))
  }
}
