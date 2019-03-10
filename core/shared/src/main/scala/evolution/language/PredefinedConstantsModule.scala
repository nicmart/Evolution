package evolution.language
import enumeratum.{ Enum, EnumEntry }
import enumeratum.EnumEntry.Lowercase

import scala.collection.immutable

trait PredefinedConstantsModule[F[_]] { self: TypesModule[F] =>
  import TypeClasses._

  abstract sealed class Constant(val qualifiedType: Qualified[Type]) extends EnumEntry with Lowercase

  object Constant extends Enum[Constant] {
    import Type._
    val values: immutable.IndexedSeq[Constant] = findValues
    val functions0: List[Constant] = List(Empty, PI)
    val nonFunctions0: List[Constant] = values.toList.filter(!functions0.contains(_))

    def unapply(s: String): Option[Constant] = withNameInsensitiveOption(s)

    // Constants

    // Math
    case object Point extends Constant(Qualified(Dbl =>: Dbl =>: Type.Point))
    case object Floor extends Constant(Qualified(Dbl =>: Integer))
    case object ToDbl extends Constant(Qualified(Integer =>: Dbl))
    case object X extends Constant(Qualified(Type.Point =>: Dbl))
    case object Y extends Constant(Qualified(Type.Point =>: Dbl))
    case object Add
        extends Constant(Qualified(List(Predicate("Semigroup", List(Var("T")))), Var("T") =>: Var("T") =>: Var("T")))
    case object Minus
        extends Constant(Qualified(List(Predicate("Semigroup", List(Var("T")))), Var("T") =>: Var("T") =>: Var("T")))
    case object Div extends Constant(Qualified(Dbl =>: Dbl =>: Dbl))
    case object Exp extends Constant(Qualified(Dbl =>: Dbl =>: Dbl))
    case object Abs extends Constant(Qualified(Dbl =>: Dbl))
    case object Sign extends Constant(Qualified(Dbl =>: Dbl))
    case object Inverse extends Constant(Qualified(Var("T") =>: Var("T")))
    case object Multiply extends Constant(Qualified(Dbl =>: Var("T") =>: Var("T")))
    case object Sin extends Constant(Qualified(Dbl =>: Dbl))
    case object Cos extends Constant(Qualified(Dbl =>: Dbl))
    case object PI extends Constant(Qualified(Dbl))
    case object Mod extends Constant(Qualified(Dbl =>: Dbl =>: Dbl))

    // Boolean
    case object If extends Constant(Qualified(Bool =>: Var("T") =>: Var("T") =>: Var("T")))
    case object Not extends Constant(Qualified(Bool =>: Bool))
    case object And extends Constant(Qualified(Bool =>: Bool =>: Bool))
    case object Or extends Constant(Qualified(Bool =>: Bool =>: Bool))
    case object Eq extends Constant(Qualified(Var("T") =>: Var("T") =>: Bool))
    case object Neq extends Constant(Qualified(Var("T") =>: Var("T") =>: Bool))
    case object GreaterThan extends Constant(Qualified(Var("T") =>: Var("T") =>: Bool))
    case object GreaterThanOrEqual extends Constant(Qualified(Var("T") =>: Var("T") =>: Bool))
    case object LessThan extends Constant(Qualified(Var("T") =>: Var("T") =>: Bool))
    case object LessThanOrEqual extends Constant(Qualified(Var("T") =>: Var("T") =>: Bool))

    // Chain
    case object Empty extends Constant(Qualified(Var("T")))
    case object Cons extends Constant(Qualified(Var("T") =>: Evo(Var("T")) =>: Evo(Var("T"))))
    case object MapEmpty extends Constant(Qualified(Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T"))))
    case object MapCons
        extends Constant(
          Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T1")) =>: Evo(Var("T2"))) =>: Evo(Var("T2"))))

    // Derived
    case object Polar extends Constant(Qualified(Dbl =>: Dbl =>: Type.Point))
    case object Constant extends Constant(Qualified(Var("T") =>: Evo(Var("T"))))
    case object Integrate extends Constant(Qualified(Var("T") =>: Evo(Var("T")) =>: Evo(Var("T"))))
    case object Solve1 extends Constant(Qualified(Evo(Var("T") =>: Var("T")) =>: Var("T") =>: Evo(Var("T"))))
    case object Solve2
        extends Constant(Qualified(Evo(Var("T") =>: Var("T") =>: Var("T")) =>: Var("T") =>: Var("T") =>: Evo(Var("T"))))
    case object Concat extends Constant(Qualified(Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T"))))
    case object Map extends Constant(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Var("T2")) =>: Evo(Var("T2"))))
    case object FlatMap
        extends Constant(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2"))))
    case object Take extends Constant(Qualified(Integer =>: Evo(Var("T")) =>: Evo(Var("T"))))
    case object ZipWith
        extends Constant(
          Qualified(Evo(Var("T1")) =>: Evo(Var("T2")) =>: (Var("T1") =>: Var("T2") =>: Var("T3")) =>: Evo(Var("T3"))))
    case object While extends Constant(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Bool) =>: Evo(Var("T1"))))
    case object Until extends Constant(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Bool) =>: Evo(Var("T1"))))
    case object InRect extends Constant(Qualified(Type.Point =>: Type.Point =>: Type.Point =>: Bool))

    // Distribution
    case object Uniform extends Constant(Qualified(Dbl =>: Dbl =>: Evo(Dbl)))
    case object UniformDiscrete extends Constant(Qualified(Dbl =>: Dbl =>: Dbl =>: Evo(Dbl)))
    case object UniformFrom extends Constant(Qualified(Integer =>: Evo(Var("T")) =>: Evo(Var("T"))))
    case object Normal extends Constant(Qualified(Dbl =>: Dbl =>: Evo(Dbl)))

    // Special functions
    case object Lift extends Constant(Qualified(Var("T")))

    // functions-only
    case object Fix extends Constant(Qualified((Var("T") =>: Var("T")) =>: Var("T")))

  }
}
