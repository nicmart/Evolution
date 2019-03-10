package evolution.language
import enumeratum.{ Enum, EnumEntry }
import enumeratum.EnumEntry.Lowercase

import scala.collection.immutable

trait PredefinedConstantsModule[F[_]] { self: TypesModule[F] =>
  import TypeClasses._

  abstract sealed class Constant(val scheme: Type, val predicates: List[Predicate]) extends EnumEntry with Lowercase

  object Constant extends Enum[Constant] {
    import Type._
    val values: immutable.IndexedSeq[Constant] = findValues
    val functions0: List[Constant] = List(Empty, PI)
    val nonFunctions0: List[Constant] = values.toList.filter(!functions0.contains(_))

    def unapply(s: String): Option[Constant] = withNameInsensitiveOption(s)

    // Constants

    // Math
    case object Point extends Constant(Dbl =>: Dbl =>: Type.Point, Nil)
    case object Floor extends Constant(Dbl =>: Integer, Nil)
    case object ToDbl extends Constant(Integer =>: Dbl, Nil)
    case object X extends Constant(Type.Point =>: Dbl, Nil)
    case object Y extends Constant(Type.Point =>: Dbl, Nil)
    case object Add extends Constant(Var("T") =>: Var("T") =>: Var("T"), List(Predicate("Semigroup", List(Var("T")))))
    case object Minus extends Constant(Var("T") =>: Var("T") =>: Var("T"), List(Predicate("Semigroup", List(Var("T")))))
    case object Div extends Constant(Dbl =>: Dbl =>: Dbl, Nil)
    case object Exp extends Constant(Dbl =>: Dbl =>: Dbl, Nil)
    case object Abs extends Constant(Dbl =>: Dbl, Nil)
    case object Sign extends Constant(Dbl =>: Dbl, Nil)
    case object Inverse extends Constant(Var("T") =>: Var("T"), Nil)
    case object Multiply extends Constant(Dbl =>: Var("T") =>: Var("T"), Nil)
    case object Sin extends Constant(Dbl =>: Dbl, Nil)
    case object Cos extends Constant(Dbl =>: Dbl, Nil)
    case object PI extends Constant(Dbl, Nil)
    case object Mod extends Constant(Dbl =>: Dbl =>: Dbl, Nil)

    // Boolean
    case object If extends Constant(Bool =>: Var("T") =>: Var("T") =>: Var("T"), Nil)
    case object Not extends Constant(Bool =>: Bool, Nil)
    case object And extends Constant(Bool =>: Bool =>: Bool, Nil)
    case object Or extends Constant(Bool =>: Bool =>: Bool, Nil)
    case object Eq extends Constant(Var("T") =>: Var("T") =>: Bool, Nil)
    case object Neq extends Constant(Var("T") =>: Var("T") =>: Bool, Nil)
    case object GreaterThan extends Constant(Var("T") =>: Var("T") =>: Bool, Nil)
    case object GreaterThanOrEqual extends Constant(Var("T") =>: Var("T") =>: Bool, Nil)
    case object LessThan extends Constant(Var("T") =>: Var("T") =>: Bool, Nil)
    case object LessThanOrEqual extends Constant(Var("T") =>: Var("T") =>: Bool, Nil)

    // Chain
    case object Empty extends Constant(Var("T"), Nil)
    case object Cons extends Constant(Var("T") =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object MapEmpty extends Constant(Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object MapCons
        extends Constant(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T1")) =>: Evo(Var("T2"))) =>: Evo(Var("T2")), Nil)

    // Derived
    case object Polar extends Constant(Dbl =>: Dbl =>: Type.Point, Nil)
    case object Constant extends Constant(Var("T") =>: Evo(Var("T")), Nil)
    case object Integrate extends Constant(Var("T") =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object Solve1 extends Constant(Evo(Var("T") =>: Var("T")) =>: Var("T") =>: Evo(Var("T")), Nil)
    case object Solve2
        extends Constant(Evo(Var("T") =>: Var("T") =>: Var("T")) =>: Var("T") =>: Var("T") =>: Evo(Var("T")), Nil)
    case object Concat extends Constant(Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object Map extends Constant(Evo(Var("T1")) =>: (Var("T1") =>: Var("T2")) =>: Evo(Var("T2")), Nil)
    case object FlatMap extends Constant(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2")), Nil)
    case object Take extends Constant(Integer =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object ZipWith
        extends Constant(
          Evo(Var("T1")) =>: Evo(Var("T2")) =>: (Var("T1") =>: Var("T2") =>: Var("T3")) =>: Evo(Var("T3")),
          Nil)
    case object While extends Constant(Evo(Var("T1")) =>: (Var("T1") =>: Bool) =>: Evo(Var("T1")), Nil)
    case object Until extends Constant(Evo(Var("T1")) =>: (Var("T1") =>: Bool) =>: Evo(Var("T1")), Nil)
    case object InRect extends Constant(Type.Point =>: Type.Point =>: Type.Point =>: Bool, Nil)

    // Distribution
    case object Uniform extends Constant(Dbl =>: Dbl =>: Evo(Dbl), Nil)
    case object UniformDiscrete extends Constant(Dbl =>: Dbl =>: Dbl =>: Evo(Dbl), Nil)
    case object UniformFrom extends Constant(Integer =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object Normal extends Constant(Dbl =>: Dbl =>: Evo(Dbl), Nil)

    // Special functions
    case object Lift extends Constant(Var("T"), Nil)

    // functions-only
    case object Fix extends Constant((Var("T") =>: Var("T")) =>: Var("T"), Nil)

  }
}
