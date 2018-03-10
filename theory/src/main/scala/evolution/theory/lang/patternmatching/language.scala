package evolution.theory.lang.patternmatching

trait Lang[F[_]] {
  def pure[A](a: A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def add(a: F[Int], b: F[Int]): F[Int]
  def zipWith[A, B, C](a: F[A], b: F[B])(f: (A, B) => C): F[C]
}

trait LangObs[F[_], T[_]] extends Lang[T] {
  def observe[A](t: T[A]): F[A]
}

object Serializer extends Lang[λ[X => String]] {
  override def pure[A](a: A): String = s"pure(${a.toString})"
  override def map[A, B](fa: String)(f: A => B): String = s"map($fa)(?)"
  override def add(a: String, b: String): String = s"add($a, $b)"
  override def zipWith[A, B, C](a: String, b: String)(f: (A, B) => C): String = s"zipWith($a, $b)(?)"
}

trait Expr[A] {
  def run[F[_]](alg: Lang[F]): F[A]
}

trait Builder extends Lang[Expr] {
  override def pure[A](a: A): Expr[A] = new Expr[A] {
    override def run[F[_]](alg: Lang[F]): F[A] = alg.pure(a)
  }
  override def map[A, B](fa: Expr[A])(f: A => B): Expr[B] = new Expr[B] {
    override def run[F[_]](alg: Lang[F]): F[B] = alg.map(fa.run(alg))(f)
  }
  override def add(a: Expr[Int], b: Expr[Int]): Expr[Int] = new Expr[Int] {
    override def run[F[_]](alg: Lang[F]): F[Int] = alg.add(a.run(alg), b.run(alg))
  }
  override def zipWith[A, B, C](a: Expr[A], b: Expr[B])(f: (A, B) => C): Expr[C] = new Expr[C] {
    override def run[F[_]](alg: Lang[F]): F[C] = alg.zipWith(a.run(alg), b.run(alg))(f)
  }
}

/**
  * A two-way transformation between two higher kinded types
  */
trait TwoWayMap[F[_], T[_]] { self =>
  def to[A](r: F[A]): T[A]
  def from[A](t: T[A]): F[A]
  def inverse: TwoWayMap[T, F] =
    new TwoWayMap[T, F] {
      override def to[A](r: T[A]): F[A] = self.from(r)
      override def from[A](t: F[A]): T[A] = self.to(t)
    }
}

/**
  * Transform an algebra of F to an algebra of T, using a TwoWayMap
  */
class TwoWayMapLang[F[_], T[_]](rr: TwoWayMap[F, T], alg: Lang[F]) extends LangObs[F, T] {
  import rr._
  override def pure[A](a: A): T[A] =
    to(alg.pure(a))
  override def map[A, B](fa: T[A])(f: A => B): T[B] =
    to(alg.map(from(fa))(f))
  override def add(a: T[Int], b: T[Int]): T[Int] =
    to(alg.add(from(a), from(b)))
  override def zipWith[A, B, C](a: T[A], b: T[B])(f: (A, B) => C): T[C] =
    to(alg.zipWith(from(a), from(b))(f))
  def observe[A](a: T[A]): F[A] =
    from(a)
}

object TwoWayMapLang {

}

object Builder extends Builder

/**
  * First optimisation: propagate constants
  */
object Const {
  sealed trait Annotation[F[_], A]
  case class Constant[F[_], A](a: A) extends Annotation[F, A]
  case class Unknown[F[_], A](fa: F[A]) extends Annotation[F, A]

  class TwoWayMapConst[F[_]](alg: Lang[F]) extends TwoWayMap[F, Annotation[F, ?]] {
    override def to[A](r: F[A]): Annotation[F, A] = Unknown(r)
    override def from[A](t: Annotation[F, A]): F[A] = t match {
      case Constant(a) => alg.pure(a)
      case Unknown(fa) => fa
    }
  }

  class Optimizer[F[_]](lang: Lang[F]) extends TwoWayMapLang[F, Annotation[F, ?]](new TwoWayMapConst[F](lang), lang) {
    override def pure[A](a: A): Annotation[F, A] =
      Constant(a)
    // The sum of two constants is a constant of the two sums
    override def add(a: Annotation[F, Int], b: Annotation[F, Int]): Annotation[F, Int] =
      (a, b) match {
        case (Constant(aconst), Constant(bconst)) => Constant(aconst + bconst)
        case _ => super.add(a, b)
      }
    // The map of a constant is a constant of the map
    override def map[A, B](fa: Annotation[F, A])(f: A => B): Annotation[F, B] =
      fa match {
        case Constant(a) => Constant(f(a))
        case _ => super.map(fa)(f)
      }
    override def zipWith[A, B, C](a: Annotation[F, A], b: Annotation[F, B])(f: (A, B) => C): Annotation[F, C] =
      (a, b) match {
        case (Constant(aconst), Constant(bconst)) => Constant(f(aconst, bconst))
        case (Constant(aconst), Unknown(fb)) => Unknown(lang.map(fb)(f(aconst, _)))
        case (Unknown(fa), Constant(bconst)) => Unknown(lang.map(fa)(f(_, bconst)))
        case _ => super.zipWith(a, b)(f)
      }
  }

  def optimise[A](expr: Expr[A]): Expr[A] = new Expr[A] {
    override def run[F[_]](alg: Lang[F]): F[A] = {
      val optimizer = new Optimizer[F](alg)
      optimizer.observe(expr.run(optimizer))
    }
  }
}

/**
  * Second optimisation: a map of a map is a map
  */
object MapFusion {
  sealed trait Annotation[F[_], A]
  case class Unknown[F[_], A](fa: F[A]) extends Annotation[F, A]
  case class Map[F[_], A, B](fa: F[A], f: A => B) extends Annotation[F, B]

  class TwoWayMapMapFusion[F[_]](alg: Lang[F]) extends TwoWayMap[F, Annotation[F, ?]] {
    override def to[A](r: F[A]): Annotation[F, A] = Unknown(r)
    override def from[A](t: Annotation[F, A]): F[A] = t match {
      case x: Map[F, c, A] => alg.map[c, A](x.fa)(x.f)
      case Unknown(fa) => fa
    }
  }

  class Optimizer[F[_]](lang: Lang[F])
    extends TwoWayMapLang[F, Annotation[F, ?]](new TwoWayMapMapFusion[F](lang), lang) {
    override def map[A, B](fa: Annotation[F, A])(f: A => B): Annotation[F, B] =
      fa match {
        case x: Map[F, c, A] => Map(x.fa, x.f andThen f)
        case _ => super.map(fa)(f)
      }
  }

  def optimise[A](expr: Expr[A]): Expr[A] = new Expr[A] {
    override def run[F[_]](alg: Lang[F]): F[A] = {
      val optimizer = new Optimizer[F](alg)
      optimizer.observe(expr.run(optimizer))
    }
  }
}

object All {
  def optimise[A](expr: Expr[A]): Expr[A] =
    Const.optimise(MapFusion.optimise(expr))
}


