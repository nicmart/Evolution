package evolution.primitive.algebra.evolution.interpreter
import Types.{ HigherKindedTypeInfo, doubleConstant, _ }
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.evolution.Evolution

class EvolutionTypedSerializer extends Evolution[F, R, Double, String, String] {
  override val chain: Chain[F, R] = new Chain[F, R] {
    override def empty[A]: R[F[A]] = R(requiredType => AnnotatedValue(requiredType, "empty"))

    override def cons[A](head: R[A], tail: R[F[A]]): R[F[A]] =
      R { requiredType =>
        AnnotatedValue(
          requiredType,
          s"cons(${head.infer(requiredType.asHigherKindedType.inner)}, ${tail.infer(requiredType)})"
        )
      }

    override def mapEmpty[A](eva: R[F[A]], eva2: R[F[A]]): R[F[A]] = R { requiredType =>
      AnnotatedValue(
        requiredType.unify(eva.infer(requiredType)).unify(eva2.infer(requiredType)),
        s"mapEmpty(${eva.infer(requiredType)}, ${eva2.infer(requiredType)})"
      )
    }

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
        val functionTypeInfo = requiredType.asFunction
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
  sealed trait TypeInfo {
    def info: String
    override def toString: String = info

    def unify(other: TypeInfo): TypeInfo = (this, other) match {
      case (Unknown(_), _) => other
      case (_, Unknown(_)) => this
      case (FunctionTypeInfo(from1, to1), FunctionTypeInfo(from2, to2)) =>
        FunctionTypeInfo(from1.unify(from2), to1.unify(to2))
      case (HigherKindedTypeInfo(label1, t1), HigherKindedTypeInfo(label2, t2)) if label1 == label2 =>
        HigherKindedTypeInfo(label1, t1.unify(t2))
      case (type1, type2) if type1 == type2 => this
      case _                                => Error(this, other)
    }

    def unify(annotatedValue: AnnotatedValue): TypeInfo =
      unify(annotatedValue.typeInfo)

    def asFunction: FunctionTypeInfo =
      this.asInstanceOf[FunctionTypeInfo]

    def asHigherKindedType: HigherKindedTypeInfo =
      this.asInstanceOf[HigherKindedTypeInfo]
  }

  case class Unknown(override final val info: String = "unknown") extends TypeInfo
  case class Error(a: TypeInfo, b: TypeInfo) extends TypeInfo {
    override def info: String = s"error($a and $b are incompatible)"
  }
  case class SimpleTypeInfo(override val info: String) extends TypeInfo
  case class FunctionTypeInfo(from: TypeInfo, to: TypeInfo) extends TypeInfo {
    override def info: String = s"$from -> $to"
  }
  case class HigherKindedTypeInfo(label: String, inner: TypeInfo) extends TypeInfo {
    override def info: String = s"$label[$inner]"
  }

  case class AnnotatedValue(typeInfo: TypeInfo, value: String) {
    override def toString = s"$value: $typeInfo"
    def wrapped = s"($value): $typeInfo"
    def unify(otherType: TypeInfo): AnnotatedValue =
      copy(typeInfo = typeInfo.unify(otherType))
  }

  case class F[T]()

  sealed trait R[T] {
    def infer(requiredType: TypeInfo): AnnotatedValue
    def unify(typeInfo: TypeInfo): R[T] =
      R { requiredType =>
        infer(requiredType).unify(typeInfo)
      }
  }

  object R {
    def apply[T](f: TypeInfo => AnnotatedValue): R[T] = new R[T] {
      override def infer(requiredType: TypeInfo): AnnotatedValue = f(requiredType)
    }

    def known[T](annotatedValue: AnnotatedValue): R[T] = R { requiredType =>
      annotatedValue.unify(requiredType)
    }
  }

  lazy val doubleConstant: TypeInfo = SimpleTypeInfo("Double")
  lazy val evolutionOfDoubles: TypeInfo = HigherKindedTypeInfo("F", doubleConstant)
  lazy val pointConstant: TypeInfo = SimpleTypeInfo("Point")
  lazy val evolutionOfPoints: TypeInfo = Types.HigherKindedTypeInfo("F", pointConstant)
}
