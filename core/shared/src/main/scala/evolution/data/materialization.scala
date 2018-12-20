package evolution.data
import evolution.geometry.Point
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.derived.Derived
import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.evolution.Evolution
import evolution.typeclass.VectorSpace

trait MaterializationModule {
  type F[T]
  type R[T]

  def evolution: Evolution[F, R]

  def materialize[T](seed: Long, ft: R[F[T]]): Iterator[T]
}

private[data] object MaterializationModuleImpl extends MaterializationModule {
  override def materialize[T](seed: Long, ft: R[F[T]]): Iterator[T] = ???

  sealed trait R[T]
  final case class Evo[T](get: F[T]) extends R[F[T]]
  final case class Dbl(d: Double) extends R[Double]
  final case class Pnt(x: R[Double], y: R[Double]) extends R[Point]
  final case class Add[T: VectorSpace](a: R[T], b: R[T]) extends R[T] {
    def map2(fa: R[T] => R[T], fb: R[T] => R[T]): Add[T] = Add(fa(a), fa(b))
  }
  final case class Multiply[T: VectorSpace](k: R[Double], t: R[T]) extends R[T] {
    def map2(fk: R[Double] => R[Double], ft: R[T] => R[T]): Multiply[T] = Multiply(fk(k), ft(t))
  }
  final case class Sin(d: R[Double]) extends R[Double]
  final case class Cos(d: R[Double]) extends R[Double]
  final case class Var0[A](name: String) extends R[A]
  final case class Shift[A](expr: R[A]) extends R[A]
  final case class Let[A, B](variable: String, value: R[A], expr: R[B]) extends R[B]
  final case class Lambda[A, B](variable: String, expr: R[B]) extends R[A => B]
  final case class App[A, B](f: R[A => B], a: R[A]) extends R[B]
  final case class Fix[A](expr: R[A => A]) extends R[A]

  sealed trait F[T]
  final case class Empty[A]() extends F[A]
  final case class Cons[A](head: R[A], tail: R[F[A]]) extends F[A]
  final case class MapEmpty[A](eva: R[F[A]], eva2: R[F[A]]) extends F[A]
  final case class MapCons[A, B](eva: R[F[A]], f: R[A => F[A] => F[B]]) extends F[B]

  override def evolution: Evolution[F, R] = new Evolution[F, R] {
    override val chain: Chain[F, R] = new Chain[F, R] {
      override def empty[A]: R[F[A]] = Evo(Empty())
      override def cons[A](head: R[A], tail: R[F[A]]): R[F[A]] = Evo(Cons(head, tail))
      override def mapEmpty[A](eva: R[F[A]], eva2: R[F[A]]): R[F[A]] = Evo(MapEmpty(eva, eva2))
      override def mapCons[A, B](eva: R[F[A]])(f: R[A => F[A] => F[B]]): R[F[B]] = Evo(MapCons(eva, f))
    }

    override val constants: Constants[R] = new Constants[R] {
      override def double(d: Double): R[Double] = Dbl(d)
      override def point(x: R[Double], y: R[Double]): R[Point] = Pnt(x, y)
      override def add[T: VectorSpace](a: R[T], b: R[T]): R[T] = Add(a, b)
      override def multiply[T: VectorSpace](k: R[Double], t: R[T]): R[T] = Multiply(k, t)
      override def sin(d: R[Double]): R[Double] = Sin(d)
      override def cos(d: R[Double]): R[Double] = Cos(d)
    }

    override val bind: Binding[R, String] = new Binding[R, String] {
      override def var0[A](name: String): R[A] = Var0(name)
      override def shift[A](expr: R[A]): R[A] = Shift(expr)
      override def let[A, B](variable: String, value: R[A], expr: R[B]): R[B] = Let(variable, value, expr)
      override def lambda[A, B](variable: String, expr: R[B]): R[A => B] = Lambda(variable, expr)
      override def app[A, B](f: R[A => B], a: R[A]): R[B] = App(f, a)
      override def fix[A](expr: R[A => A]): R[A] = Fix(expr)
    }
    override val distribution: Distribution[F, R] = ???
    override val derived: Derived[F, R] = ???
  }

  private def usedVars[T](rt: R[T]): Set[Int] =
    rt match {
      case Evo(get)            => usedVars(get)
      case Dbl(d)              => Set.empty
      case Pnt(x, y)           => usedVars(x) ++ usedVars(y)
      case Add(a, b)           => usedVars(a) ++ usedVars(b)
      case Multiply(k, t)      => usedVars(k) ++ usedVars(t)
      case Sin(d)              => usedVars(d)
      case Cos(d)              => usedVars(d)
      case Var0(name)          => Set(0)
      case Shift(expr)         => usedVars(expr).map(_ + 1)
      case Let(_, value, expr) => usedVars(value) ++ usedVars(expr).map(_ - 1).filter(_ >= 0)
      case Lambda(_, expr)     => usedVars(expr).map(_ - 1).filter(_ >= 0)
      case App(f, a)           => usedVars(f) ++ usedVars(a)
      case Fix(expr)           => usedVars(expr).map(_ - 1).filter(_ >= 0)
    }

  private def usedVars[T](evo: F[T]): Set[Int] = evo match {
    case Empty()             => Set.empty
    case Cons(head, tail)    => usedVars(head) ++ usedVars(tail)
    case MapEmpty(eva, eva2) => usedVars(eva) ++ usedVars(eva2)
    case MapCons(eva, f)     => usedVars(eva) ++ usedVars(f)
  }

  private def assign[A, B](rt: R[A], variable: Int, value: R[B]): R[A] =
    rt match {
      case Evo(get)                => Evo(assign(get, variable, value))
      case Dbl(d)                  => Dbl(d)
      case Pnt(x, y)               => Pnt(assign(x, variable, value), assign(y, variable, value))
      case add @ Add(a, b)         => add.map2(assign(_, variable, value), assign(_, variable, value))
      case mult @ Multiply(k, t)   => mult.map2(assign(_, variable, value), assign(_, variable, value))
      case Sin(d)                  => Sin(assign(d, variable, value))
      case Cos(d)                  => Cos(assign(d, variable, value))
      case Var0(name)              => if (variable == 0) value.asInstanceOf[R[A]] else Var0(name)
      case Shift(expr)             => Shift(assign(expr, variable - 1, value))
      case Let(v, letVal, letExpr) => Let(v, assign(letVal, variable, value), assign(letExpr, variable, value))
      case Lambda(v, expr)         => Lambda(v, assign(expr, variable, value))
      case App(f, a)               => App(assign(f, variable, value), assign(a, variable, value))
      case Fix(expr)               => Fix(assign(expr, variable, value))
    }

  private def assign[A, B](fa: F[A], variable: Int, value: R[B]): F[A] =
    fa match {
      case Empty()             => Empty()
      case Cons(head, tail)    => Cons(assign(head, variable, value), assign(tail, variable, value))
      case MapEmpty(eva, eva2) => MapEmpty(assign(eva, variable, value), assign(eva2, variable, value))
      case MapCons(eva, f)     => MapCons(assign(eva, variable, value), assign(f, variable, value))
    }

  private def isConstantLambda[A, B](f: R[A => B]): Option[R[B]] =
    f match {
      case Lambda(variable, expr) => Some(expr).filter(usedVars(_).contains(0))
      case _                      => None
    }
}
