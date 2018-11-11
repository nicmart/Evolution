package evolution.primitive.algebra.evolution.interpreter
import Types._
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.evolution.Evolution

class EvolutionTypedSerializer extends Evolution[F, R, Double, String, String] {
  override val chain: Chain[F, R] = new Chain[F, R] {
    override def empty[A]: R[F[A]] = R(requiredType => AnnotatedValue(requiredType, "empty"))
    override def cons[A](head: R[A], tail: R[F[A]]): R[F[A]] = ???
    override def mapEmpty[A](eva: R[F[A]], eva2: R[F[A]]): R[F[A]] = ???
    override def mapCons[A, B](eva: R[F[A]])(f: R[A => F[A] => F[B]]): R[F[B]] = ???
  }

  override val constants: Constants[R, Double] = new Constants[R, Double] {
    override def double(d: Double): R[Double] =
      R.known(AnnotatedValue(doubleConstant, d.toString))
    override def point(x: R[Double], y: R[Double]): R[Point] =
      R.known(AnnotatedValue(pointConstant, s"point(${x.infer(doubleConstant)}, ${y.infer(doubleConstant)})"))
    override def add[T: Semigroup](a: R[T], b: R[T]): R[T] = ???
  }

  override val bind: Binding[R, String, String] = new Binding[R, String, String] {
    override def v(name: String): String = ???
    override def var0[A]: R[A] = ???
    override def shift[A](expr: R[A]): R[A] = ???
    override def let[A, B](variable: String, value: R[A], expr: R[B]): R[B] = ???

    override def lambda[A, B](variable: String, expr: R[B]): R[A => B] =
      R(requiredType => {
        val functionTypeInfo = requiredType.asInstanceOf[FunctionTypeInfo[A, B]]
        AnnotatedValue(functionTypeInfo, s"$variable: ${functionTypeInfo.from} -> ${expr.infer(functionTypeInfo.to)}")
      })

    override def app[A, B](f: R[A => B], a: R[A]): R[B] = R(bType => {
      val fType = FunctionTypeInfo(a.infer(Unknown()).typeInfo, bType)
      AnnotatedValue(bType, s"app(${f.infer(fType).wrapped}, ${a.infer(Unknown())})")
    })

    override def fix[A](expr: R[A => A]): R[A] = ???
  }
}

object Types {
  sealed abstract class TypeInfo[T] {
    def info: String
    override def toString: String = info
    def unify(other: TypeInfo[T]): TypeInfo[T] = (this, other) match {
      case (Unknown(_), _)                  => other
      case (_, Unknown(_))                  => this
      case (type1, type2) if type1 == type2 => this
      case _                                => Error(this, other)
    }
  }

  case class Unknown[T](override final val info: String = "unknown") extends TypeInfo[T]
  case class Error[T](a: TypeInfo[T], b: TypeInfo[T]) extends TypeInfo[T] {
    override def info: String = s"error($a and $b are incompatible)"
  }
  case class SimpleTypeInfo[T](override val info: String) extends TypeInfo[T]
  case class FunctionTypeInfo[A, B](from: TypeInfo[A], to: TypeInfo[B]) extends TypeInfo[A => B] {
    override def info: String = s"$from -> $to"
  }

  case class AnnotatedValue[T](typeInfo: TypeInfo[T], value: String) {
    override def toString = s"$value: $typeInfo"
    def wrapped = s"($value): $typeInfo"
  }

  case class F[T]()

  sealed trait R[T] {
    def infer(requiredType: TypeInfo[T]): AnnotatedValue[T]
  }

  object R {
    def apply[T](f: TypeInfo[T] => AnnotatedValue[T]): R[T] = new R[T] {
      override def infer(requiredType: TypeInfo[T]): AnnotatedValue[T] = f(requiredType)
    }

    def known[T](annotatedValue: AnnotatedValue[T]): R[T] = R { requiredType =>
      annotatedValue.copy(typeInfo = requiredType.unify(annotatedValue.typeInfo))
    }
  }

  lazy val evolutionOfDoubles: TypeInfo[F[Double]] = SimpleTypeInfo("F[Double]")
  lazy val evolutionOfPoints: TypeInfo[F[Point]] = SimpleTypeInfo("F[Point]")
  lazy val doubleConstant: TypeInfo[Double] = SimpleTypeInfo("Double")
  lazy val pointConstant: TypeInfo[Point] = SimpleTypeInfo("Point")
}
