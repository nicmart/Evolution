package evolution.theory.lang.list

import scala.language.higherKinds

trait BindingLang {
  type ~>[A, B]
  type Env[A]
  def var0[A]: A ~> Env[A]
  def varS[A, B](expr: Env[A]): B ~> Env[A]
  def ap[A, B](f: Env[A ~> B])(a: Env[A]): Env[B]
  def ap2[A, B, C](f: Env[A ~> (B ~> C)])(a: Env[A], b: Env[B]): Env[C] = ap(ap(f)(a))(b)
  def fix[A](f: A ~> Env[A]): A
}

trait Lang extends BindingLang { self =>
  type F0[A]
  type C0[A]
  final type F[A] = Env[F0[A]]
  final type C[A] = Env[C0[A]]
  def next[X]: LangNext[X]
  def value[A](a: A): C[A]
  def empty[A]: F[A]
  def cons[A](a: C[A], tail: F[A]): F[A]
  def mapEmpty[A](fa1: F[A], fa2: F[A]): F[A]
  def mapCons[A, B](fa1: F[A])(f: C0[A] ~> (F0[A] ~> F[B])): F[B]

  // Fine here, we are still in the applicative domain
  def mapC[A, B](ca: C[A])(f: A => B): C[B]
  def mapF[A, B](ca: F[A])(f: A => B): F[B]
  // Not sure if this is more a zipWith rather than a map2
  def map2F[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z]
  def map2C[A, B, Z](fa: C[A], fb: C[B])(f: (A, B) => Z): C[Z]

  trait LangNext[X] extends Lang {
    final type Env[A] = X ~> self.Env[A]
    final type ~>[A, B] = self.~>[A, B]
    final type F0[A] = self.F0[A]
    final type C0[A] = self.C0[A]
    val x: Env[X] = self.var0[X]
    def up[A](a: self.Env[A]): Env[A] = self.varS[A, X](a)
    // note that, assuming Lang.Env[A] = A, then
    // Lang.var0[F[A]]: F[A] ~> F[A]
    // LangNext[Lang.F[A]].F[A]
    //   = LangNext.Env[Lang.F[A]]
    //   = Lang.F[A] ~> Lang.F[A]
    // So var0[F[A]]: LangNext.F[A]
  }
}

object Lang {
  trait Base extends Lang {
    type Env[A] = A
  }
}

class Expressions(val lang: Lang.Base) {
  import lang._

  def constant[A](a: A): F[A] = {
    val b1 = next[F[A]]
    fix(b1.cons[A](varS(value(a)), b1.x))
  }


  def integrate: C[Double] ~> (F[Double] ~> F[Double]) = {
    // velocity evolution
    val b1 = next[F[Double]]
    // from constant
    val b2 = b1.next[C[Double]]
    // integrate recursive call
    val b3 = b2.next[C[Double] ~> (F[Double] ~> F[Double])]

    val y3: (C[Double] ~> (F[Double] ~> F[Double])) ~> (C[Double] ~> (F[Double] ~> Env[C[Double] ~> (F[Double] ~> F[Double])])) = b3.x

    // tail of mapCons
    val b4 = b3.next[F[Double]]
    // head of mapCons
    val b5 = b4.next[C[Double]]

    type LastEnv[X] = C[Double] ~> (F[Double] ~> ((C[Double] ~> (F[Double] ~> F[Double])) ~> (C[Double] ~> (F[Double] ~> Env[X]))))

    val from: LastEnv[C[Double]] =
      b5.up(b4.up(b3.up(b2.x)))
    val vhead: LastEnv[C[Double]] = b5.x
    val vtail: LastEnv[F[Double]] = b5.up(b4.x)
    val integrateRec: LastEnv[C[Double] ~> (F[Double] ~> F[Double])] = b5.up(b4.up(b3.x))

    fix[C[Double] ~> (F[Double] ~> F[Double])](
      // MapCons here needs a C[Double] ~> (F[Double] ~> F[Double])  as "first argument"
      // and a C[D] ~> F[D] ~> F[D] as "second argument", BUT here C and F are the ones in b3, oh no!!!

      b3.mapCons(b3.up(b2.up(b1.x)))(
        b5.cons(
          from,
          b5.ap2(integrateRec)(
            b5.map2C(vhead, from)(_ + _),
            vtail
          )
        )
      )
      //ap2[C[Double], F[Double], F[Double]](b3.mapCons[Double, Double](???)(???))(???, ???)
    )
  }
}

trait Serializer extends Lang {
  type ~>[A, B] = A => B
  type F0[A] = String
  def var0[E, A]: ((A, E)) => A =
    { case (a, e) => a }
  def varS[E, A, B](expr: E => A): ((B, E)) => A =
    { case (b, e) => expr(e) }
  def fix[E, A](f: (A, E) => A): E => A =
    e => f(fix(f)(e), e)
  def empty[E, A]: E => String =
    _ => "empty"
  def cons[E, A](a: A, tail: E => String): E => String =
    e => s"cons(${a.toString}, ${tail(e)})"
}