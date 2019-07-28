package evolution.language
import cats.implicits._
import cats.mtl.FunctorRaise
import cats.{ Applicative, Monad }
import enumeratum.EnumEntry.Lowercase
import enumeratum.{ Enum, EnumEntry }
import evolution.data.ExpressionModule
import evolution.geometry.Point

import scala.collection.immutable
import scala.language.higherKinds

trait PredefinedConstantsModule[F[_]] { self: TypesModule[F] with ExpressionModule[F] =>
  import Type._
  import TypeClasses._

  abstract sealed class Constant(val qualifiedType: Qualified[Type]) extends EnumEntry with Lowercase

  abstract sealed class Constant0(qualifiedType: Qualified[Type])
      extends Constant(qualifiedType)
      with EnumEntry
      with Lowercase {

    def compile[M[_]](tpe: Qualified[Type])(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]]
  }

  object Constant0 extends Enum[Constant0] {
    val values: immutable.IndexedSeq[Constant0] = findValues

    case object PI extends Constant0(Qualified(Dbl)) {
      def compile[M[_]](tpe: Qualified[Type])(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Expr.Dbl(Math.PI).pure[M].widen
    }

    case object Empty extends Constant0(Qualified(Var("T"))) {
      def compile[M[_]](tpe: Qualified[Type])(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Expr.Empty().pure[M].widen
    }

    case object Noise extends Constant0(Qualified(Evo(Type.Point =>: Dbl))) {
      override def compile[M[_]](
        tpe: TypeClasses.Qualified[Type]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Expr.Noise().asExpr.pure[M].widen
    }

    case object OctaveNoise extends Constant0(Qualified(Evo(Integer =>: Dbl =>: Type.Point =>: Dbl))) {
      override def compile[M[_]](
        tpe: TypeClasses.Qualified[Type]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Expr.OctaveNoise().asExpr.pure[M].widen
    }

    def unapply(s: String): Option[Constant0] = withNameInsensitiveOption(s)
  }

  abstract sealed class Constant1(qualifiedType: Qualified[Type])
      extends Constant(qualifiedType)
      with EnumEntry
      with Lowercase {
    def compile[M[_]](x: Typed[Expr[_]])(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]]
  }

  abstract sealed class Constant1Plain(qualifiedType: Qualified[Type]) extends Constant1(qualifiedType) {
    def compilePlain(x: Expr[_]): Expr[_]
    override def compile[M[_]](x: Typed[Expr[_]])(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
      compilePlain(x.value).pure[M].widen
  }

  object Constant1 extends Enum[Constant1] {
    val values: immutable.IndexedSeq[Constant1] = findValues

    case object X extends Constant1Plain(Qualified(Type.Point =>: Dbl)) {
      override def compilePlain(x: Expr[_]): Expr[_] = Expr.X(x.asExpr)
    }

    case object Y extends Constant1Plain(Qualified(Type.Point =>: Dbl)) {
      override def compilePlain(x: Expr[_]): Expr[_] = Expr.Y(x.asExpr)
    }

    case object Floor extends Constant1Plain(Qualified(Dbl =>: Integer)) {
      override def compilePlain(x: Expr[_]): Expr[_] = Expr.Floor(x.asExpr)
    }

    case object ToDbl extends Constant1Plain(Qualified(Integer =>: Dbl)) {
      override def compilePlain(x: Expr[_]): Expr[_] = Expr.ToDbl(x.asExpr)
    }

    case object Abs extends Constant1Plain(Qualified(Dbl =>: Dbl)) {
      override def compilePlain(x: Expr[_]): Expr[_] = Expr.Abs(x.asExpr)
    }

    case object Sign extends Constant1Plain(Qualified(Dbl =>: Dbl)) {
      override def compilePlain(x: Expr[_]): Expr[_] = Expr.Sign(x.asExpr)
    }

    case object Norm extends Constant1Plain(Qualified(Type.Point =>: Type.Dbl)) {
      override def compilePlain(x: Expr[_]): Expr[_] = Expr.Norm(x.asExpr)
    }

    case object Versor extends Constant1Plain(Qualified(Type.Point =>: Type.Point)) {
      override def compilePlain(x: Expr[_]): Expr[_] = Expr.Versor(x.asExpr)
    }

    case object Inverse extends Constant1(Qualified(Var("T") =>: Var("T"))) {
      override def compile[M[_]](x: Typed[Expr[_]])(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        x.tpe match {
          // Overload - for evolutions
          case Type.Evo(tpe) =>
            Type.group[M](tpe).map { group =>
              Expr.Map(x.value.asExprF, Expr.Lambda("t", Expr.Inverse(Expr.Var("t"), group)))
            }

          case tpe =>
            Type.group[M](tpe).map { g =>
              Expr.Inverse(x.value.asExpr, g)
            }
        }
    }

    case object Sin extends Constant1Plain(Qualified(Dbl =>: Dbl)) {
      override def compilePlain(x: Expr[_]): Expr[_] = Expr.Sin(x.asExpr)
    }

    case object Cos extends Constant1Plain(Qualified(Dbl =>: Dbl)) {
      override def compilePlain(x: Expr[_]): Expr[_] = Expr.Cos(x.asExpr)
    }

    case object Not extends Constant1Plain(Qualified(Bool =>: Bool)) {
      override def compilePlain(x: Expr[_]): Expr[_] = Expr.Not(x.asExpr)
    }

    case object Constant extends Constant1Plain(Qualified(Var("T") =>: Evo(Var("T")))) {
      override def entryName: String = "@"
      override def compilePlain(x: Expr[_]): Expr[_] = Expr.Constant(x.asExpr)
    }

    case object Fix extends Constant1Plain(Qualified((Var("T") =>: Var("T")) =>: Var("T"))) {
      override def compilePlain(x: Expr[_]): Expr[_] = Expr.Fix(x.asExpr[Any => Any])
    }

    case object Flatten extends Constant1Plain(Qualified(Evo(Evo(Var("T"))) =>: Evo(Var("T")))) {
      override def compilePlain(x: Expr[_]): Expr[_] = Expr.Flatten(x.asExprF[F[Any]])
    }

    case object Parallel extends Constant1Plain(Qualified(Evo(Evo(Var("T"))) =>: Evo(Var("T")))) {
      override def compilePlain(x: Expr[_]): Expr[_] = Expr.Parallel(x.asExprF[F[Any]])
    }

    case object Derive extends Constant1(Qualified(Evo(Var("T")) =>: Evo(Var("T")))) {

      override def compile[M[_]](x: Typed[Expr[_]])(
        implicit M: Monad[M],
        E: FunctorRaise[M, String]
      ): M[Expr[_]] =
        Type.group[M](x.tpe).map(group => Expr.Derive(x.value.asExprF, group))
    }

    def unapply(s: String): Option[Constant1] = withNameInsensitiveOption(s)
  }

  abstract sealed class Constant2(qualifiedType: Qualified[Type])
      extends Constant(qualifiedType)
      with EnumEntry
      with Lowercase {
    def compile[M[_]](x: Typed[Expr[_]], y: Typed[Expr[_]])(
      implicit M: Monad[M],
      E: FunctorRaise[M, String]
    ): M[Expr[_]]
  }

  abstract sealed class Constant2Plain(qualifiedType: Qualified[Type]) extends Constant2(qualifiedType) {
    override def compile[M[_]](
      x: Typed[Expr[_]],
      y: Typed[Expr[_]]
    )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
      compilePlain(x.value, y.value).pure[M].widen

    def compilePlain(x: Expr[_], y: Expr[_]): Expr[_]
  }

  object Constant2 extends Enum[Constant2] {
    val values: immutable.IndexedSeq[Constant2] = findValues

    case object Point extends Constant2Plain(Qualified(Dbl =>: Dbl =>: Type.Point)) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Pnt(x.asExpr, y.asExpr)
    }

    case object LiftedPoint extends Constant2Plain(Qualified(Evo(Dbl) =>: Evo(Dbl) =>: Evo(Type.Point))) {
      override def entryName: String = "@point"
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] =
        Expr.LiftedPnt(x.asExprF, y.asExprF).asExpr[F[_]]
    }

    case object Polar extends Constant2Plain(Qualified(Dbl =>: Dbl =>: Type.Point)) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Polar(x.asExpr, y.asExpr)
    }

    case object LiftedPolar extends Constant2Plain(Qualified(Evo(Dbl) =>: Evo(Dbl) =>: Evo(Type.Point))) {
      override def entryName: String = "@polar"
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] =
        Expr.LiftedPolar(x.asExprF, y.asExprF)
    }

    case object Multiply extends Constant2(Qualified(Dbl =>: Var("T") =>: Var("T"))) {
      override def compile[M[_]](
        x: Typed[Expr[_]],
        y: Typed[Expr[_]]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Type.vectorSpace[M](y.tpe).map(vs => Expr.Multiply(x.value.asExpr, y.value.asExpr, vs))
    }

    case object LiftedMultiply extends Constant2(Qualified(Evo(Dbl) =>: Evo(Var("T")) =>: Evo(Var("T")))) {
      override def compile[M[_]](
        x: Typed[Expr[_]],
        y: Typed[Expr[_]]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        for {
          tpe <- Type.unwrapF[M](y.tpe)
          vs <- Type.vectorSpace[M](tpe)
        } yield Expr.LiftedMultiply(x.value.asExprF, y.value.asExprF, vs).asExpr[F[_]]
    }

    case object Add
        extends Constant2(Qualified(List(Predicate("Semigroup", List(Var("T")))), Var("T") =>: Var("T") =>: Var("T"))) {
      override def compile[M[_]](
        x: Typed[Expr[_]],
        y: Typed[Expr[_]]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Type.group[M](y.tpe).map(vs => Expr.Add(x.value.asExpr, y.value.asExpr, vs))
    }

    case object LiftedAdd extends Constant2(Qualified(Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T")))) {
      override def compile[M[_]](
        x: Typed[Expr[_]],
        y: Typed[Expr[_]]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        for {
          tpe <- Type.unwrapF[M](x.tpe)
          sg <- Type.group[M](tpe)
        } yield Expr.LiftedAdd(x.value.asExprF, y.value.asExprF, sg).asExpr[F[_]]
    }

    case object Minus
        extends Constant2(Qualified(List(Predicate("Semigroup", List(Var("T")))), Var("T") =>: Var("T") =>: Var("T"))) {
      override def compile[M[_]](
        x: Typed[Expr[_]],
        y: Typed[Expr[_]]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Type.group[M](x.tpe).map(group => Expr.Add(x.value.asExpr, Expr.Inverse(y.value.asExpr, group), group))
    }

    case object Div extends Constant2Plain(Qualified(Dbl =>: Dbl =>: Dbl)) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Div(x.asExpr, y.asExpr)
    }

    case object Exp extends Constant2Plain(Qualified(Dbl =>: Dbl =>: Dbl)) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Exp(x.asExpr, y.asExpr)
    }

    case object Mod extends Constant2Plain(Qualified(Dbl =>: Dbl =>: Dbl)) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Mod(x.asExpr, y.asExpr)
    }

    case object And extends Constant2Plain(Qualified(Bool =>: Bool =>: Bool)) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.And(x.asExpr, y.asExpr)
    }

    case object Or extends Constant2Plain(Qualified(Bool =>: Bool =>: Bool)) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Or(x.asExpr, y.asExpr)
    }

    case object Eq extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool)) {
      override def compile[M[_]](
        x: Typed[Expr[_]],
        y: Typed[Expr[_]]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Type.eqTypeClass[M](y.tpe).map(eq => Expr.Equals(x.value.asExpr, y.value.asExpr, eq))

    }

    case object Neq extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool)) {
      override def compile[M[_]](
        x: Typed[Expr[_]],
        y: Typed[Expr[_]]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Type.eqTypeClass[M](y.tpe).map(eq => Expr.Neq(x.value.asExpr, y.value.asExpr, eq))
    }

    case object GreaterThan extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool)) {
      override def compile[M[_]](
        x: Typed[Expr[_]],
        y: Typed[Expr[_]]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Type.order[M](y.tpe).map(order => Expr.GreaterThan(x.value.asExpr, y.value.asExpr, order))
    }

    case object GreaterThanOrEqual extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool)) {
      override def compile[M[_]](
        x: Typed[Expr[_]],
        y: Typed[Expr[_]]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Type.order[M](y.tpe).map(order => Expr.GreaterThanOrEqual(x.value.asExpr, y.value.asExpr, order))
    }

    case object LessThan extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool)) {
      override def compile[M[_]](
        x: Typed[Expr[_]],
        y: Typed[Expr[_]]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Type.order[M](y.tpe).map(order => Expr.LessThan(x.value.asExpr, y.value.asExpr, order))
    }

    case object LessThanOrEqual extends Constant2(Qualified(Var("T") =>: Var("T") =>: Bool)) {
      override def compile[M[_]](
        x: Typed[Expr[_]],
        y: Typed[Expr[_]]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Type.order[M](y.tpe).map(order => Expr.LessThanOrEqual(x.value.asExpr, y.value.asExpr, order))
    }

    case object Cons extends Constant2Plain(Qualified(Var("T") =>: Evo(Var("T")) =>: Evo(Var("T")))) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Cons(x.asExpr, y.asExprF)
    }

    case object MapEmpty extends Constant2Plain(Qualified(Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T")))) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.MapEmpty(x.asExprF, y.asExprF)
    }

    case object MapCons
        extends Constant2Plain(
          Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T1")) =>: Evo(Var("T2"))) =>: Evo(Var("T2")))
        ) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.MapCons(
        x.asExprF,
        y.asExpr[Any => F[Any] => F[Any]]
      )
    }

    case object WithFirst
        extends Constant2Plain(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2")))) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.WithFirst(x.asExprF, y.asExpr[Any => F[Any]])
    }

    case object WithFirst2
        extends Constant2Plain(
          Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2")))
        ) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] =
        Expr.WithFirst2(x.asExprF, y.asExpr[Any => Any => F[Any]])
    }

    case object WithFirst3
        extends Constant2Plain(
          Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Var("T1") =>: Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2")))
        ) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] =
        Expr.WithFirst3(x.asExprF, y.asExpr[Any => Any => Any => F[Any]])
    }

    case object Integrate extends Constant2(Qualified(Var("T") =>: Evo(Var("T")) =>: Evo(Var("T")))) {
      override def compile[M[_]](
        x: Typed[Expr[_]],
        y: Typed[Expr[_]]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Type.group[M](x.tpe).map(group => Expr.Integrate(x.value.asExpr, y.value.asExprF, group))
    }

    case object Solve1 extends Constant2(Qualified(Evo(Var("T") =>: Var("T")) =>: Var("T") =>: Evo(Var("T")))) {
      override def compile[M[_]](
        x: Typed[Expr[_]],
        y: Typed[Expr[_]]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Type
          .group[M](y.tpe)
          .map(group => Expr.Solve1[y.tpe.Out](x.value.asExprF[y.tpe.Out => y.tpe.Out], y.value.asExpr, group))
    }

    case object Concat extends Constant2Plain(Qualified(Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T")))) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Concat(x.asExprF, y.asExprF)
    }

    case object Map extends Constant2Plain(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Var("T2")) =>: Evo(Var("T2")))) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Map(x.asExprF, y.asExpr[Any => Any])
    }

    case object MapWithDerivative
        extends Constant2(Qualified((Var("T1") =>: Var("T1") =>: Var("T2")) =>: Evo(Var("T1")) =>: Evo(Var("T2")))) {

      override def compile[M[_]](
        x: Typed[Expr[_]],
        y: Typed[Expr[_]]
      )(implicit M: Monad[M], E: FunctorRaise[M, String]): M[Expr[_]] =
        Type.group[M](x.tpe).map { group =>
          Expr.MapWithDerivative(x.value.asExprF, y.value.asExpr[Any => Any => Any], group)
        }
    }

    case object FlatMap
        extends Constant2Plain(Qualified(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2")))) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] =
        Expr.FlatMap(x.asExprF, y.asExpr[Any => F[Any]])
    }

    case object Take extends Constant2Plain(Qualified(Integer =>: Evo(Var("T")) =>: Evo(Var("T")))) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Take(x.asExpr, y.asExprF)
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

    case object Uniform extends Constant2Plain(Qualified(Dbl =>: Dbl =>: Evo(Dbl))) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Uniform(x.asExpr, y.asExpr)
    }

    case object UniformFrom extends Constant2Plain(Qualified(Integer =>: Evo(Var("T")) =>: Evo(Var("T")))) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.UniformFrom(x.asExpr, y.asExprF)
    }

    case object Normal extends Constant2Plain(Qualified(Dbl =>: Dbl =>: Evo(Dbl))) {
      override def compilePlain(x: Expr[_], y: Expr[_]): Expr[_] = Expr.Normal(x.asExpr, y.asExpr)
    }

    def unapply(s: String): Option[Constant2] = withNameInsensitiveOption(s)
  }

  abstract sealed class Constant3(qualifiedType: Qualified[Type])
      extends Constant(qualifiedType)
      with EnumEntry
      with Lowercase {
    def compile[M[_]](x: Typed[Expr[_]], y: Typed[Expr[_]], z: Typed[Expr[_]])(
      implicit M: Monad[M],
      E: FunctorRaise[M, String]
    ): M[Expr[_]]
  }

  abstract sealed class Constant3Plain(qualifiedType: Qualified[Type]) extends Constant3(qualifiedType) {
    def compilePlain(x: Expr[_], y: Expr[_], z: Expr[_]): Expr[_]
    def compile[M[_]](x: Typed[Expr[_]], y: Typed[Expr[_]], z: Typed[Expr[_]])(
      implicit M: Monad[M],
      E: FunctorRaise[M, String]
    ): M[Expr[_]] = compilePlain(x.value, y.value, z.value).pure[M].widen
  }

  object Constant3 extends Enum[Constant3] {
    val values: immutable.IndexedSeq[Constant3] = findValues

    case object SmoothStep extends Constant3Plain(Qualified(Dbl =>: Dbl =>: Dbl =>: Dbl)) {
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

    case object Range extends Constant3Plain(Qualified(Dbl =>: Dbl =>: Dbl =>: Evo(Dbl))) {
      def compilePlain(x: Expr[_], y: Expr[_], z: Expr[_]): Expr[_] = Expr.Range(x.asExpr, y.asExpr, z.asExpr)
    }

    case object Solve2
        extends Constant3(
          Qualified(Evo(Var("T") =>: Var("T") =>: Var("T")) =>: Var("T") =>: Var("T") =>: Evo(Var("T")))
        ) {
      override def compile[M[_]](x: Typed[Expr[_]], y: Typed[Expr[_]], z: Typed[Expr[_]])(
        implicit M: Monad[M],
        E: FunctorRaise[M, String]
      ): M[Expr[_]] =
        Type.group[M](y.tpe).map { group =>
          Expr.Solve2[y.tpe.Out](
            x.value.asExprF[y.tpe.Out => y.tpe.Out => y.tpe.Out],
            y.value.asExpr[y.tpe.Out],
            z.value.asExpr[y.tpe.Out],
            group
          )
        }
    }

    case object InRect extends Constant3Plain(Qualified(Type.Point =>: Type.Point =>: Type.Point =>: Bool)) {
      override def compilePlain(x: Expr[_], y: Expr[_], z: Expr[_]): Expr[_] =
        Expr.InRect(x.asExpr[Point], y.asExpr[Point], z.asExpr[Point])
    }
    case object If extends Constant3Plain(Qualified(Bool =>: Var("T") =>: Var("T") =>: Var("T"))) {
      override def compilePlain(x: Expr[_], y: Expr[_], z: Expr[_]): Expr[_] =
        Expr.IfThen(x.asExpr, y, z.asExpr)
    }

    case object UniformDiscrete extends Constant3Plain(Qualified(Dbl =>: Dbl =>: Dbl =>: Evo(Dbl))) {
      override def compilePlain(x: Expr[_], y: Expr[_], z: Expr[_]): Expr[_] =
        Expr.UniformDiscrete(x.asExpr, y.asExpr, z.asExpr)
    }

    def unapply(s: String): Option[Constant3] = withNameInsensitiveOption(s)
  }

  object Constant {
    val values: Seq[Constant] = Constant0.values ++ Constant1.values ++ Constant2.values ++ Constant3.values
  }

  implicit class CastingOps(value: Expr[_]) {
    def asExpr[T]: Expr[T] = value.asInstanceOf[Expr[T]]
    def asExprF[T]: Expr[F[T]] = value.asInstanceOf[Expr[F[T]]]
  }

  def fromEither[M[_], T](e: Either[String, T])(implicit M: Applicative[M], E: FunctorRaise[M, String]): M[T] =
    e.fold(E.raise, M.pure)

}
