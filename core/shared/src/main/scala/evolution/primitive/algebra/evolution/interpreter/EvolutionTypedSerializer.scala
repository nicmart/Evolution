package evolution.primitive.algebra.evolution.interpreter
import Types.{ HigherKindedTypeInfo, doubleConstant, intConstant, _ }
import cats.kernel.{ Eq, Semigroup }
import evolution.geometry.Point
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.derived.Derived
import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.evolution.Evolution
import evolution.typeclass.VectorSpace

class EvolutionTypedSerializer extends Evolution[F, R] {
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

    override def mapCons[A, B](eva: R[F[A]])(f: R[A => F[A] => F[B]]): R[F[B]] =
      R { requiredType =>
        val evaType = requiredType.asHigherKindedType.copy(inner = Unknown())
        val fRequiredType =
          FunctionTypeInfo(
            eva.infer(evaType).typeInfo.asHigherKindedType.inner,
            FunctionTypeInfo(
              eva.infer(evaType).typeInfo,
              requiredType
            )
          )
        AnnotatedValue(
          requiredType,
          s"mapCons(${eva.infer(evaType)}, ${f.infer(fRequiredType)})"
        )
      }
  }

  override val constants: Constants[R] = new Constants[R] {
    override def int(n: Int): R[Int] =
      R.known(AnnotatedValue(intConstant, n.toString))
    override def double(d: Double): R[Double] =
      R.known(AnnotatedValue(doubleConstant, d.toString))
    override def point(x: R[Double], y: R[Double]): R[Point] =
      R.known(AnnotatedValue(pointConstant, s"point(${x.infer(doubleConstant)}, ${y.infer(doubleConstant)})"))
    override def add[T: VectorSpace](a: R[T], b: R[T]): R[T] = R { requiredType =>
      val annotatedA @ AnnotatedValue(aType, aValue) = a.infer(requiredType)
      val annotatedB @ AnnotatedValue(bType, bValue) = b.infer(requiredType)
      val unifiedType = requiredType.unify(aType).unify(bType)
      AnnotatedValue(unifiedType, s"add($annotatedA, $annotatedB)")
    }
    override def sin(d: R[Double]): R[Double] = R { requiredType =>
      val annotatedD = d.infer(requiredType)
      AnnotatedValue(requiredType, s"sin($annotatedD)")
    }

    override def cos(d: R[Double]): R[Double] = R { requiredType =>
      val annotatedD = d.infer(requiredType)
      AnnotatedValue(requiredType, s"sin($annotatedD)")
    }
    override def multiply[T: VectorSpace](k: R[Double], t: R[T]): R[T] = R { requiredType =>
      val annotatedK = k.infer(doubleConstant)
      val annotatedT = t.infer(requiredType)
      AnnotatedValue(requiredType, s"multiply($annotatedK, $annotatedT)")
    }
    override def eq[T: Eq](a: R[T], b: R[T]): R[Boolean] = ???
    override def ifThen[T](condition: R[Boolean], a: R[T], b: R[T]): R[T] = ???
  }

  override val bind: Binding[R, String] = new Binding[R, String] {
    override def var0[A](name: String): R[A] = R.unknown("var0")
    override def shift[A](expr: R[A]): R[A] = expr.mapValue(value => value.copy(value = s"shift($value)"))
    override def let[A, B](variable: String, value: R[A], expr: R[B]): R[B] = R { requiredBType =>
      val annotatedA @ AnnotatedValue(aType, aValue) = value.infer(Unknown())
      val annotatedB @ AnnotatedValue(bType, bValue) = expr.infer(requiredBType)
      val unifiedBType = bType.unify(requiredBType)
      AnnotatedValue(unifiedBType, s"let($variable, $annotatedA, $annotatedB)")
    }

    override def lambda[A, B](variable: String, expr: R[B]): R[A => B] =
      R(requiredType => {
        val functionTypeInfo = requiredType.asFunction
        val inferredType = functionTypeInfo.copy(to = expr.infer(functionTypeInfo.to).typeInfo)
        AnnotatedValue(inferredType, s"$variable -> ${expr.infer(functionTypeInfo.to).value}")
      })

    override def app[A, B](f: R[A => B], a: R[A]): R[B] = R(bType => {
      val fType = FunctionTypeInfo(a.infer(Unknown()).typeInfo, bType)
      AnnotatedValue(bType, s"app(${f.infer(fType)}, ${a.infer(Unknown())})")
    })

    override def fix[A](expr: R[A => A]): R[A] = R { requiredAType =>
      val expectedFuncType = FunctionTypeInfo(requiredAType, requiredAType)
      val annotatedFunc @ AnnotatedValue(FunctionTypeInfo(from, to), exprValue) = expr.infer(expectedFuncType)
      AnnotatedValue(requiredAType, s"fix($annotatedFunc)")
    }
  }

  override val distribution: Distribution[F, R] = new Distribution[F, R] {
    override def uniform(from: R[Double], to: R[Double]): R[F[Double]] = R { required =>
      val expectedHKType @ HigherKindedTypeInfo(label, inner) = required
      val annotatedFrom @ AnnotatedValue(fromType, fromValue) = from.infer(inner)
      val annotatedTo @ AnnotatedValue(toType, toValue) = to.infer(inner)
      AnnotatedValue(required, s"uniform($annotatedFrom, $annotatedTo)")
    }
  }

  override val derived: Derived[F, R] = new Derived[F, R] {
    override def cartesian(x: R[F[Double]], y: R[F[Double]]): R[F[Point]] = R { required =>
      val annotatedX = x.infer(evolutionOfDoubles)
      val annotatedY = y.infer(evolutionOfDoubles)
      AnnotatedValue(required.unify(evolutionOfPoints), s"uniform($annotatedX, $annotatedY)")
    }
    override def constant[A](a: R[A]): R[F[A]] = R { required =>
      val expectedHKType @ HigherKindedTypeInfo(label, inner) = required
      val annotatedA @ AnnotatedValue(aType, aValue) = a.infer(inner)
      AnnotatedValue(required, s"constant($annotatedA)")
    }
    override def polar(radius: R[F[Double]], angle: R[F[Double]]): R[F[Point]] = R { required =>
      val annotatedRadius = radius.infer(evolutionOfDoubles)
      val annotatedAngle = angle.infer(evolutionOfDoubles)
      AnnotatedValue(required.unify(evolutionOfPoints), s"polar($annotatedRadius, $annotatedAngle)")
    }

    override def integrate[A: VectorSpace](start: R[A], speed: R[F[A]]): R[F[A]] = R { required =>
      val expectedHKType @ HigherKindedTypeInfo(label, inner) = required
      val annotatedStart = start.infer(inner)
      val annotatedSpeed = speed.infer(required)
      AnnotatedValue(required, s"integrate($annotatedStart, $annotatedSpeed)")
    }
    override def map[A, B](fa: R[F[A]], f: R[A => B]): R[F[B]] = R { required =>
      val expectedHKType @ HigherKindedTypeInfo(label, inner) = required
      val annotatedFa = fa.infer(Unknown())
      val HigherKindedTypeInfo(labelFa, aTypeInfo) = annotatedFa.typeInfo
      val annotatedF = f.infer(FunctionTypeInfo(aTypeInfo, inner))
      AnnotatedValue(required, s"map($annotatedFa, $annotatedF)")
    }

    override def concat[A](fa1: R[F[A]], fa2: R[F[A]]): R[F[A]] = R { required =>
      AnnotatedValue(required, s"concat(${fa1.infer(required)}, ${fa2.infer(required)})")
    }

    override def flatMap[A, B](fa: R[F[A]], f: R[A => F[B]]): R[F[B]] = R { required =>
      val expectedHKType @ HigherKindedTypeInfo(label, inner) = required
      val annotatedFa = fa.infer(Unknown())
      val annotatedF = f.infer(FunctionTypeInfo(annotatedFa.typeInfo, expectedHKType))
      AnnotatedValue(required, s"flatMap($annotatedFa, $annotatedF)")
    }
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
    def mapValue(f: AnnotatedValue => AnnotatedValue): R[T] = R { t =>
      f(infer(t))
    }
  }

  object R {
    def apply[T](f: TypeInfo => AnnotatedValue): R[T] = new R[T] {
      override def infer(requiredType: TypeInfo): AnnotatedValue = f(requiredType)
    }

    def known[T](annotatedValue: AnnotatedValue): R[T] = R { requiredType =>
      annotatedValue.unify(requiredType)
    }

    def unknown[T](value: String): R[T] = R { requiredType =>
      AnnotatedValue(requiredType, value)
    }
  }

  lazy val doubleConstant: TypeInfo = SimpleTypeInfo("Double")
  lazy val intConstant: TypeInfo = SimpleTypeInfo("Int")
  lazy val evolutionOfDoubles: TypeInfo = HigherKindedTypeInfo("F", doubleConstant)
  lazy val pointConstant: TypeInfo = SimpleTypeInfo("Point")
  lazy val evolutionOfPoints: TypeInfo = Types.HigherKindedTypeInfo("F", pointConstant)
}
