package evolution.theory.lang.list

import scala.language.higherKinds

trait BindingLang {
  type ~>[A, B]
  type Env[A]
  def var0[A]: A ~> Env[A]
  def varS[A, B](expr: Env[A]): B ~> Env[A]
  def ap[A, B](f: A ~> B)(a: A): B
  def ap2[A, B, C](f: A ~> (B ~> C))(a: A, b: B): C = ap(ap(f)(a))(b)
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

  def mapC[A, B](ca: C[A])(f: A => B): C[B]
  def mapF[A, B](ca: F[A])(f: A => B): F[B]

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
    // b1 works on F[Double] ~> *
    val b1 = next[F[Double]]
    // This compiles
    val y1: F[Double] ~> Env[F[Double]] = b1.x
    val y11: b1.Env[F[Double]] = b1.x
    // b2 works on C[Double] ~> (F[Double] ~> *)
    val b2 = b1.next[C[Double]]
    // This compiles
    val y2: C[Double] ~> b1.Env[C[Double]] = b2.x
    val y21: C[Double] ~> (F[Double] ~> Env[C[Double]]) = b2.x
    // b3 works on (C[Double] ~> (F[Double] ~> F[Double])) ~> (C[Double] ~> (F[Double] ~> *))
    // In b3, b2.var0[C[Double] ~> (F[Double] ~> F[Double])] is integrate
    // b3.x
    // b2.varS(b1.var0[C[Double]]) is the "from" b2.varS(b2.x)
    // b2.varS(b1.varS(var0[F[Double]])) is the speed (b2.varS(b1.varS(b1.x)))
    val b3 = b2.next[C[Double] ~> (F[Double] ~> F[Double])]
    //val y3: (C[Double] ~> (F[Double] ~> F[Double])) ~> (C[Double] ~> (F[Double] ~> Env[C[Double]])) = b3.x
    val y3: (C[Double] ~> (F[Double] ~> F[Double])) ~> (C[Double] ~> (F[Double] ~> Env[C[Double] ~> (F[Double] ~> F[Double])])) = b3.x
    //val integrateVar = b3.x
    //val fromVar = b2.varS[C[Double] ~> C[Double], C[Double] ~> (F[Double] ~> F[Double])](b1.var0[C[Double]])

    // Inside fix we need (C[D] ~> (F[D] ~> F[D])) ~> Env[(C[D] ~> F[D] ~> F[D])]
    // Inside fix we need (C[D] ~> (F[D] ~> F[D])) ~> (C[D] ~> (F[D] ~> F[D]))
    // The F[] for mapCons must then be (C[D] ~> F[D] ~> F[D]) ~> C[D] ~> F[D] ~> F[_]
    // That means we first push F[D], then C[D], and then (C[D] ~> F[D] ~> F[D])

    val b11 = next[C[Double] ~> (F[Double] ~> F[Double])]
    //val b21 = b11.next[]

    b1.mapCons(b1.x)(???)
    b2.mapCons[Double, Double](b2.up(b1.x))(???)

    // tail of mapCons
    val b4 = b3.next[F[Double]]
    // head of mapCons
    val b5 = b4.next[C[Double]]

    fix[C[Double] ~> (F[Double] ~> F[Double])](
      // MapCons here needs a C[Double] ~> (F[Double] ~> F[Double])  as "first argument"
      // and a C[D] ~> F[D] ~> F[D] as "second argument", BUT here C and F are the ones in b3, oh no!!!

      b3.mapCons(b3.up(b2.up(b1.x)))(
        b5.cons[Double](
          b5.up(b4.up(b3.up(b2.x))),
          b5.up(b4.x)
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