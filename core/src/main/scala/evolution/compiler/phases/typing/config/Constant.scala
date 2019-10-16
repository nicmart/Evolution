package evolution.compiler.phases.typing.config

import cats.implicits._
import enumeratum.EnumEntry.Lowercase
import enumeratum.{ Enum, EnumEntry }
import evolution.compiler.phases.typing.config.Constant._
import evolution.compiler.types.TypeT._
import evolution.compiler.types.Type
import evolution.compiler.types.TypeT
import evolution.compiler.types.TypeClasses._
import evolution.compiler.types.{ Type, TypeClasses, Typed }
import evolution.compiler.expression.Expr
import evolution.geometry.Point
import evolution.materialization.Evolution
import scala.collection.immutable

/**
 * The modeling here is a bit loose.
 * Constants are domain concepts and not only "config" concepts.
 * The list of concrete instances is a "config".
 * But it is not easy to split the two now (mainly because we are using enumeratum, and that requires the traits to be sealed)
 */
abstract sealed class Constant(val qualifiedType: Qualified[Type]) extends EnumEntry with Lowercase

abstract sealed class Constant0(qualifiedType: Qualified[Type])
    extends Constant(qualifiedType)
    with EnumEntry
    with Lowercase {

  def compile(tpe: Qualified[Type]): Either[String, Expr[_]]
}

object Constant0 extends Enum[Constant0] {
  lazy val values: immutable.IndexedSeq[Constant0] = findValues

  case object PI extends Constant0(Qualified(TypeT.Double)) {
    def compile(tpe: Qualified[Type]): Either[String, Expr[_]] =
      Expr.Dbl(Math.PI).asRight
  }

  case object Empty extends Constant0(Qualified(Var("T"))) {
    def compile(tpe: Qualified[Type]): Either[String, Expr[_]] =
      Expr.FromList(Expr.Lst(Nil)).asRight
  }

  case object Noise extends Constant0(Qualified(Evo(TypeT.Point =>: TypeT.Double))) {
    override def compile(tpe: TypeClasses.Qualified[Type]): Either[String, Expr[_]] =
      Expr.Noise().asRight
  }

  case object OctaveNoise extends Constant0(Qualified(Evo(Integer =>: TypeT.Double =>: TypeT.Point =>: TypeT.Double))) {
    override def compile(tpe: TypeClasses.Qualified[Type]): Either[String, Expr[_]] =
      Expr.OctaveNoise().asRight
  }

  def unapply(s: String): Option[Constant0] = withNameInsensitiveOption(s)
}

abstract sealed class Constant1(qualifiedType: Qualified[Type])
    extends Constant(qualifiedType)
    with EnumEntry
    with Lowercase {
  def compile(x: Typed[_], outType: Type): Either[String, Expr[_]]
}

abstract sealed class Constant1Plain(qualifiedType: Qualified[Type]) extends Constant1(qualifiedType) {
  def compilePlain(x: Expr[_]): Expr[_]
  override def compile(x: Typed[_], out: Type): Either[String, Expr[_]] =
    compilePlain(x.value).asRight
}

object Constant1 extends Enum[Constant1] {
  val values: immutable.IndexedSeq[Constant1] = findValues

  case object X extends Constant1Plain(Qualified(TypeT.Point =>: TypeT.Double)) {
    override def compilePlain(x: Expr[_]): Expr[_] = Expr.X(x.asExpr)
  }

  case object Y extends Constant1Plain(Qualified(TypeT.Point =>: TypeT.Double)) {
    override def compilePlain(x: Expr[_]): Expr[_] = Expr.Y(x.asExpr)
  }

  case object Floor extends Constant1Plain(Qualified(TypeT.Double =>: Integer)) {
    override def compilePlain(x: Expr[_]): Expr[_] = Expr.Floor(x.asExpr)
  }

  case object ToDbl extends Constant1Plain(Qualified(Integer =>: TypeT.Double)) {
    override def compilePlain(x: Expr[_]): Expr[_] = Expr.ToDouble(x.asExpr)
  }

  case object Abs extends Constant1Plain(Qualified(TypeT.Double =>: TypeT.Double)) {
    override def compilePlain(x: Expr[_]): Expr[_] = Expr.Abs(x.asExpr)
  }

  case object Sign extends Constant1Plain(Qualified(TypeT.Double =>: TypeT.Double)) {
    override def compilePlain(x: Expr[_]): Expr[_] = Expr.Sign(x.asExpr)
  }

  case object Norm extends Constant1Plain(Qualified(TypeT.Point =>: TypeT.Double)) {
    override def compilePlain(x: Expr[_]): Expr[_] = Expr.Norm(x.asExpr)
  }

  case object Versor extends Constant1Plain(Qualified(TypeT.Point =>: TypeT.Point)) {
    override def compilePlain(x: Expr[_]): Expr[_] = Expr.Versor(x.asExpr)
  }

  case object Inverse extends Constant1(Qualified(Var("T") =>: Var("T"))) {
    override def compile(x: Typed[_], out: Type): Either[String, Expr[_]] =
      TypingConfig.invertible(x.tpe).map(inv => Expr.Inverse(x.value, inv))
  }

  case object Sin extends Constant1Plain(Qualified(TypeT.Double =>: TypeT.Double)) {
    override def compilePlain(x: Expr[_]): Expr[_] = Expr.Sin(x.asExpr)
  }

  case object Cos extends Constant1Plain(Qualified(TypeT.Double =>: TypeT.Double)) {
    override def compilePlain(x: Expr[_]): Expr[_] = Expr.Cos(x.asExpr)
  }

  case object Not extends Constant1Plain(Qualified(Bool =>: Bool)) {
    override def compilePlain(x: Expr[_]): Expr[_] = Expr.Not(x.asExpr)
  }

  case object Constant extends Constant1Plain(Qualified(Var("T") =>: Evo(Var("T")))) {
    override def entryName: String = "const"
    override def compilePlain(x: Expr[_]): Expr[_] = Expr.Constant(x.asExpr)
  }

  case object FromList extends Constant1Plain(Qualified(Lst(Var("T")) =>: Evo(Var("T")))) {
    def compilePlain(x: Expr[_]): Expr[_] = Expr.FromList(x.asExpr)
  }

  case object Fix extends Constant1Plain(Qualified((Var("T") =>: Var("T")) =>: Var("T"))) {
    override def compilePlain(x: Expr[_]): Expr[_] = Expr.Fix(x.asExpr[Any => Any])
  }

  case object Flatten extends Constant1Plain(Qualified(Evo(Evo(Var("T"))) =>: Evo(Var("T")))) {
    override def compilePlain(x: Expr[_]): Expr[_] = Expr.Flatten(x.asExprF[Evolution[Any]])
  }

  case object Parallel extends Constant1Plain(Qualified(Evo(Evo(Var("T"))) =>: Evo(Var("T")))) {
    override def compilePlain(x: Expr[_]): Expr[_] = Expr.Parallel(x.asExprF[Evolution[Any]])
  }

  case object Derive extends Constant1(Qualified(isInvertSemigroup("T"), Evo(Var("T")) =>: Evo(Var("T")))) {

    override def compile(x: Typed[_], out: Type): Either[String, Expr[_]] =
      for {
        innerType <- x.tpe.unwrapEvo
        add <- TypingConfig.additive(innerType, innerType, innerType)
        inv <- TypingConfig.invertible(innerType)
      } yield Expr.Derive(x.value.asExprF, add, inv)
  }

  case object UniformChoice extends Constant1Plain(Qualified(Lst(Var("T")) =>: Evo(Var("T")))) {
    def compilePlain(x: Expr[_]): Expr[_] = Expr.UniformChoice(x.asExpr[List[Any]])
  }

  def unapply(s: String): Option[Constant1] = withNameInsensitiveOption(s)
}

abstract sealed class Constant2(qualifiedType: Qualified[Type])
    extends Constant(qualifiedType)
    with EnumEntry
    with Lowercase {
  def compile(x: Typed[_], y: Typed[_], outType: Type): Either[String, Expr[_]]
}

abstract sealed class Constant2Plain(qualifiedType: Qualified[Type]) extends Constant2(qualifiedType) {
  override def compile(
    x: Typed[_],
    y: Typed[_],
    out: Type
  ): Either[String, Expr[_]] =
    compilePlain(x.value, y.value).asRight

  def compilePlain(x: Expr[_], y: Expr[_]): Expr[_]
}

object Constant2 extends Enum[Constant2] {
  lazy val values: immutable.IndexedSeq[Constant2] = findValues

  case object Point extends Constant2Plain(Qualified(TypeT.Double =>: TypeT.Double =>: TypeT.Point)) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Pnt(x.asExpr, y.asExpr)
  }

  case object LiftedPoint
      extends Constant2Plain(Qualified(Evo(TypeT.Double) =>: Evo(TypeT.Double) =>: Evo(TypeT.Point))) {
    override def entryName: String = "@point"
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] =
      Expr.LiftedPnt(x.asExprF, y.asExprF).asExpr[Evolution[_]]
  }

  case object Polar extends Constant2Plain(Qualified(TypeT.Double =>: TypeT.Double =>: TypeT.Point)) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Polar(x.asExpr, y.asExpr)
  }

  case object LiftedPolar
      extends Constant2Plain(Qualified(Evo(TypeT.Double) =>: Evo(TypeT.Double) =>: Evo(TypeT.Point))) {
    override def entryName: String = "@polar"
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] =
      Expr.LiftedPolar(x.asExprF, y.asExprF)
  }

  case object Multiply
      extends Constant2(
        Qualified(List(Predicate("Mult", List(Var("A"), Var("B"), Var("C")))), Var("A") =>: Var("B") =>: Var("C"))
      ) {
    override def compile(x: Typed[_], y: Typed[_], out: Type): Either[String, Expr[_]] =
      TypingConfig.multiplicative(x.tpe, y.tpe, out).map(sg => Expr.Multiply(x.value, y.value, sg))
  }

  case object Add
      extends Constant2(
        Qualified(List(Predicate("Add", List(Var("A"), Var("B"), Var("C")))), Var("A") =>: Var("B") =>: Var("C"))
      ) {
    override def compile(x: Typed[_], y: Typed[_], out: Type): Either[String, Expr[_]] =
      TypingConfig.additive(x.tpe, y.tpe, out).map(sg => Expr.Add(x.value, y.value, sg))
  }

  case object Minus extends Constant2(Qualified(isInvertSemigroup("T"), Var("T") =>: Var("T") =>: Var("T"))) {
    override def compile(x: Typed[_], y: Typed[_], out: Type): Either[String, Expr[_]] =
      for {
        inv <- TypingConfig.invertible(x.tpe)
        add <- TypingConfig.additive(x.tpe, x.tpe, x.tpe)
      } yield Expr.Minus(x.value, y.value, add, inv)
  }

  case object Div extends Constant2Plain(Qualified(TypeT.Double =>: TypeT.Double =>: TypeT.Double)) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Div(x.asExpr, y.asExpr)
  }

  case object Exp extends Constant2Plain(Qualified(TypeT.Double =>: TypeT.Double =>: TypeT.Double)) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Exp(x.asExpr, y.asExpr)
  }

  case object Mod extends Constant2Plain(Qualified(TypeT.Double =>: TypeT.Double =>: TypeT.Double)) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Mod(x.asExpr, y.asExpr)
  }

  case object And extends Constant2Plain(Qualified(Bool =>: Bool =>: Bool)) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.And(x.asExpr, y.asExpr)
  }

  case object Or extends Constant2Plain(Qualified(Bool =>: Bool =>: Bool)) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Or(x.asExpr, y.asExpr)
  }

  case object Eq extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool)) {
    override def compile(x: Typed[_], y: Typed[_], out: Type): Either[String, Expr[_]] =
      TypingConfig.equable(y.tpe).map(eq => Expr.Equals(x.value, y.value, eq))

  }

  case object Neq extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool)) {
    override def compile(x: Typed[_], y: Typed[_], out: Type): Either[String, Expr[_]] =
      TypingConfig.equable(y.tpe).map(eq => Expr.Neq(x.value, y.value, eq))
  }

  case object GreaterThan extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool)) {
    override def compile(x: Typed[_], y: Typed[_], out: Type): Either[String, Expr[_]] =
      TypingConfig.comparable(y.tpe).map(order => Expr.GreaterThan(x.value, y.value, order))
  }

  case object GreaterThanOrEqual extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool)) {
    override def compile(x: Typed[_], y: Typed[_], out: Type): Either[String, Expr[_]] =
      TypingConfig.comparable(y.tpe).map(order => Expr.GreaterThanOrEqual(x.value, y.value, order))
  }

  case object LessThan extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool)) {
    override def compile(x: Typed[_], y: Typed[_], out: Type): Either[String, Expr[_]] =
      TypingConfig.comparable(y.tpe).map(order => Expr.LessThan(x.value, y.value, order))
  }

  case object LessThanOrEqual extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool)) {
    override def compile(x: Typed[_], y: Typed[_], out: Type): Either[String, Expr[_]] =
      TypingConfig.comparable(y.tpe).map(order => Expr.LessThanOrEqual(x.value, y.value, order))
  }

  case object Cons extends Constant2Plain(Qualified(Var("T") =>: Evo(Var("T")) =>: Evo(Var("T")))) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Cons(x.asExpr, y.asExprF)
  }

  case object WithFirst
      extends Constant2Plain(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2")))) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] =
      Expr.WithFirst(x.asExprF, y.asExpr[Any => Evolution[Any]])
  }

  case object Integrate extends Constant2(Qualified(Var("T") =>: Evo(Var("T")) =>: Evo(Var("T")))) {
    override def compile(x: Typed[_], y: Typed[_], out: Type): Either[String, Expr[_]] =
      TypingConfig.additive(x.tpe, x.tpe, x.tpe).map(add => Expr.Integrate(x.value, y.value.asExprF, add))
  }

  case object Solve1 extends Constant2(Qualified(Evo(Var("T") =>: Var("T")) =>: Var("T") =>: Evo(Var("T")))) {
    override def compile(x: Typed[_], y: Typed[_], out: Type): Either[String, Expr[_]] =
      TypingConfig.additive(y.tpe, y.tpe, y.tpe).map(add => Expr.Solve1(x.value.asExprF, y.value, add))
  }

  case object Roll extends Constant2Plain(Qualified(Evo(Var("T") =>: Var("T")) =>: Var("T") =>: Evo(Var("T")))) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] =
      Expr.Roll(x.asExprF[Any => Any], y)
  }

  case object Concat extends Constant2Plain(Qualified(Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T")))) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Concat(x.asExprF, y.asExprF)
  }

  case object Map extends Constant2Plain(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Var("T2")) =>: Evo(Var("T2")))) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Map(x.asExprF, y.asExpr[Any => Any])
  }

  case object SlidingMap
      extends Constant2Plain(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Var("T1") =>: Var("T2")) =>: Evo(Var("T2")))) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.SlidingMap(x.asExprF, y.asExpr[Any => Any => Any])
  }

  case object Iterate extends Constant2Plain(Qualified((Var("T") =>: Var("T")) =>: Var("T") =>: Evo(Var("T")))) {
    override def compilePlain(f: Expr[_], start: Expr[_]): Expr[_] =
      Expr.Iterate(f.asExpr[Any => Any], start)
  }

  case object MapWithDerivative
      extends Constant2(
        Qualified(
          isInvertSemigroup("T1"),
          Evo(Var("T1")) =>: (Var("T1") =>: Var("T1") =>: Var("T2")) =>: Evo(Var("T2"))
        )
      ) {

    override def compile(x: Typed[_], f: Typed[_], out: Type): Either[String, Expr[_]] =
      for {
        innerType <- x.tpe.unwrapEvo
        inv <- TypingConfig.invertible(innerType)
        add <- TypingConfig.additive(innerType, innerType, innerType)
      } yield Expr.MapWithDerivative(x.value.asExprF, f.value.asExpr[Any => Any => Any], add, inv)
  }

  case object FlatMap
      extends Constant2Plain(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2")))) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] =
      Expr.FlatMap(x.asExprF, y.asExpr[Any => Evolution[Any]])
  }

  case object Take extends Constant2Plain(Qualified(Evo(Var("T")) =>: Integer =>: Evo(Var("T")))) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Take(x.asExprF, y.asExpr)
  }

  case object Grouped extends Constant2Plain(Qualified(Evo(Var("T")) =>: Integer =>: Evo(Lst(Var("T"))))) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Grouped(x.asExprF, y.asExpr)
  }

  // syntactic sugar
  case object Filter extends Constant2Plain(Qualified(Evo(Var("T")) =>: (Var("T") =>: Bool) =>: Evo(Var("T")))) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = {
      val varName = y.freshVarName("__element")
      Expr.FlatMap(
        x.asExprF,
        Expr.Lambda(
          varName,
          Expr.IfThen(
            Expr.App(y.asExpr[Any => Boolean], Expr.Var(varName)),
            Expr.Cons(Expr.Var(varName), Expr.Empty()),
            Expr.Empty()
          )
        )
      )
    }
  }

  case object While extends Constant2Plain(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Bool) =>: Evo(Var("T1")))) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.TakeWhile(x.asExprF, y.asExpr[Any => Boolean])
  }

  case object Until extends Constant2Plain(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Bool) =>: Evo(Var("T1")))) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = {
      val t = y.freshVarName("t")
      Expr.TakeWhile(x.asExprF, Expr.Lambda(t, Expr.Not(Expr.App(y.asExpr[Any => Boolean], Expr.Var(t)))))
    }
  }

  case object Uniform extends Constant2Plain(Qualified(TypeT.Double =>: TypeT.Double =>: Evo(TypeT.Double))) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Uniform(x.asExpr, y.asExpr)
  }

  case object UniformFrom extends Constant2Plain(Qualified(Integer =>: Evo(Var("T")) =>: Evo(Var("T")))) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.UniformFrom(x.asExpr, y.asExprF)
  }

  case object Normal extends Constant2Plain(Qualified(TypeT.Double =>: TypeT.Double =>: Evo(TypeT.Double))) {
    override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Normal(x.asExpr, y.asExpr)
  }

  def unapply(s: String): Option[Constant2] = withNameInsensitiveOption(s)
}

abstract sealed class Constant3(qualifiedType: Qualified[Type])
    extends Constant(qualifiedType)
    with EnumEntry
    with Lowercase {
  def compile(x: Typed[_], y: Typed[_], z: Typed[_], outType: Type): Either[String, Expr[_]]
}

abstract sealed class Constant3Plain(qualifiedType: Qualified[Type]) extends Constant3(qualifiedType) {
  def compilePlain(x: Expr[_], y: Expr[_], z: Expr[_]): Expr[_]
  def compile(x: Typed[_], y: Typed[_], z: Typed[_], out: Type): Either[String, Expr[_]] =
    compilePlain(x.value, y.value, z.value).asRight
}

object Constant3 extends Enum[Constant3] {
  lazy val values: immutable.IndexedSeq[Constant3] = findValues

  case object SmoothStep
      extends Constant3Plain(Qualified(TypeT.Double =>: TypeT.Double =>: TypeT.Double =>: TypeT.Double)) {
    override def compilePlain(x: Expr[_], y: Expr[_], z: Expr[_]): Expr[_] =
      Expr.SmoothStep(x.asExpr, y.asExpr, z.asExpr)
  }

  case object ZipWith
      extends Constant3Plain(
        Qualified(Evo(Var("T1")) =>: Evo(Var("T2")) =>: (Var("T1") =>: Var("T2") =>: Var("T3")) =>: Evo(Var("T3")))
      ) {
    override def compilePlain(x: Expr[_], y: Expr[_], z: Expr[_]): Expr[_] =
      Expr.ZipWith(x.asExprF, y.asExprF, z.asExpr[Any => Any => Any])
  }

  case object Range
      extends Constant3Plain(Qualified(TypeT.Double =>: TypeT.Double =>: TypeT.Double =>: Evo(TypeT.Double))) {
    def compilePlain(x: Expr[_], y: Expr[_], z: Expr[_]): Expr[_] = Expr.Range(x.asExpr, y.asExpr, z.asExpr)
  }

  case object Solve2
      extends Constant3(
        Qualified(Evo(Var("T") =>: Var("T") =>: Var("T")) =>: Var("T") =>: Var("T") =>: Evo(Var("T")))
      ) {
    override def compile(x: Typed[_], y: Typed[_], z: Typed[_], out: Type): Either[String, Expr[_]] =
      TypingConfig.additive(y.tpe, y.tpe, y.tpe).map { add =>
        Expr.Solve2(x.value.asExprF, y.value, z.value, add)
      }
  }

  case object Roll2
      extends Constant3Plain(
        Qualified(Evo(Var("T") =>: Var("T") =>: Var("T")) =>: Var("T") =>: Var("T") =>: Evo(Var("T")))
      ) {
    override def compilePlain(f: Expr[_], a0: Expr[_], a1: Expr[_]): Expr[_] =
      Expr.Roll2(f.asExprF[Any => Any => Any], a0, a1)
  }

  case object Iterate2
      extends Constant3Plain(
        Qualified((Var("T") =>: Var("T") =>: Var("T")) =>: Var("T") =>: Var("T") =>: Evo(Var("T")))
      ) {
    def compilePlain(f: Expr[_], a0: Expr[_], a1: Expr[_]): Expr[_] =
      Expr.Iterate2(f.asExpr, a0.asExpr, a1.asExpr)
  }

  case object InRect extends Constant3Plain(Qualified(TypeT.Point =>: TypeT.Point =>: TypeT.Point =>: Bool)) {
    override def compilePlain(x: Expr[_], y: Expr[_], z: Expr[_]): Expr[_] =
      Expr.InRect(x.asExpr[Point], y.asExpr[Point], z.asExpr[Point])
  }
  case object If extends Constant3Plain(Qualified(Bool =>: Var("T") =>: Var("T") =>: Var("T"))) {
    override def compilePlain(x: Expr[_], y: Expr[_], z: Expr[_]): Expr[_] =
      Expr.IfThen(x.asExpr, y, z.asExpr)
  }

  case object UniformDiscrete
      extends Constant3Plain(Qualified(TypeT.Double =>: TypeT.Double =>: TypeT.Double =>: Evo(TypeT.Double))) {
    override def compilePlain(x: Expr[_], y: Expr[_], z: Expr[_]): Expr[_] =
      Expr.UniformDiscrete(x.asExpr, y.asExpr, z.asExpr)
  }

  def unapply(s: String): Option[Constant3] = withNameInsensitiveOption(s)
}

object Constant {
  lazy val values: Seq[Constant] = Constant0.values ++ Constant1.values ++ Constant2.values ++ Constant3.values

  def isInvertSemigroup(varName: String) = List(
    Predicate("Add", List(Var(varName), Var(varName), Var(varName))),
    Predicate("Invertible", List(Var(varName)))
  )

  implicit class CastingOps(value: Expr[_]) {
    def asExpr[T]: Expr[T] = value.asInstanceOf[Expr[T]]
    def asExprF[T]: Expr[Evolution[T]] = value.asInstanceOf[Expr[Evolution[T]]]
  }
}
