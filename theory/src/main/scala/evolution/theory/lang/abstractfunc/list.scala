package evolution.theory.lang.abstractfunc

import evolution.theory.lang.abstractfunc.Types._
import evolution.theory.lang.patternmatching.Const.{Annotation, Constant, Unknown}
import evolution.theory.lang.patternmatching.{Lang, TwoWayMap}

trait ListAlgebra[F[_], S[_]] {
  def value[A](a: A): S[A]
  def empty[A]: F[A]
  def cons[A](head: S[A], tail: F[A]): F[A]
  def flatMap[A, B](fa: F[A])(f: FMap[A, B]): F[B]
  def fix[A](f: A ~> A): F[A]
}

class ExtendedListAlgebra[F[_], S[_]](alg: ListAlgebra[F, S]) {
  import alg._
  def point[A](a: A): F[A] =
    cons(value(a), empty)
  def map[A, B](fa: F[A])(f: SMap[A, B]): F[B] =
    flatMap(fa)(new FMap[A, B] {
      override def run[F2[_], S2[_]](alg2: ListAlgebra[F2, S2]): S2[A] => F2[B] =
        sa => alg2.cons(f.run(alg2)(sa), alg2.empty)
    })
  def concat[A](fa1: F[A], fa2: F[A]): F[A] = ???
}

trait ListExpr[A] {
  def run[F[_], S[_]](alg: ListAlgebra[F, S]): F[A]
}

trait ~>[A, B] {
  def run[F[_], S[_]](alg: ListAlgebra[F, S]): F[A] => F[B]
}

trait FMap[A, B] {
  def run[F[_], S[_]](alg: ListAlgebra[F, S]): S[A] => F[B]
}

trait SMap[A, B] {
  def run[F[_], S[_]](alg: ListAlgebra[F, S]): S[A] => S[B]
}

trait SFMap[A, B] {
  def run[F[_], S[_]](alg: ListAlgebra[F, S]): (S[A], F[A]) => F[B]
}

object UnitInterpreter extends ListAlgebra[ConstUnit, ConstUnit] {
  override def value[A](a: A): Unit = ()
  override def empty[A]: Unit = ()
  override def cons[A](head: Unit, tail: Unit): Unit = ()
  override def flatMap[A, B](fa: Unit)(f: FMap[A, B]): Unit = ()
  override def fix[A](f: ~>[A, A]): Unit = ()
}

object LazyStreamInterpreter extends ListAlgebra[LazyStream, Id] {
  override def empty[A]: LazyStream[A] = () => Stream.empty
  override def cons[A](head: A, tail: LazyStream[A]): LazyStream[A] = () => head #:: tail()
  override def fix[A](f: A ~> A): LazyStream[A] = f.run(this)(() => fix(f)())
  override def value[A](a: A): Id[A] = a
  override def flatMap[A, B](fa: LazyStream[A])(f: FMap[A, B]): LazyStream[B] =
    () => fa().flatMap(a => f.run(this)(a)())
}

object StreamInterpreter extends ListAlgebra[Stream, Id] {
  override def empty[A]: Stream[A] = Stream.empty
  override def cons[A](head: A, tail: Stream[A]): Stream[A] = head #:: tail
  override def fix[A](f: ~>[A, A]): Stream[A] = f.run(LazyStreamInterpreter)(() => fix(f))()
  override def value[A](a: A): Id[A] = a
  override def flatMap[A, B](fa: Stream[A])(f: FMap[A, B]): Stream[B] =
    fa.flatMap(a => f.run(this)(a))
}

object Types {
  type LazyStream[A] = () => Stream[A]
  type StringConst[A] = String
  type Id[A] = A
  type ConstUnit[A] = Unit
  type ConstBool[A] = Boolean
  type Const[A, B] = B
}

object StringInterpreter extends ListAlgebra[StringConst, StringConst] {
  override def empty[A]: String = "Nil"
  override def cons[A](head: String, tail: String): String = s"$head::$tail"
  override def fix[A](f: A ~> A): String = s"fix($$self => ${f.run(StringInterpreter)("$self")})"
  override def value[A](a: A): StringConst[A] = a.toString
  override def flatMap[A, B](fa: StringConst[A])(f: FMap[A, B]): StringConst[B] =
    s"flatMap($fa)(x => ${f.run(this)("x")})"
}

trait TwoWayMap[F[_], T[_]] {
  def to[A](r: F[A]): T[A]
  def from[A](t: T[A]): F[A]
}

object TwoWayMap {
  def identity[F[_]]: TwoWayMap[F, F] = new TwoWayMap[F, F] {
    override def to[A](r: F[A]): F[A] = r
    override def from[A](t: F[A]): F[A] = t
  }
}

class TwoWayMapLang[F1[_], F2[_], S[_]](map: TwoWayMap[F1, F2], alg: ListAlgebra[F1, S]) extends ListAlgebra[F2, S] {
  override def value[A](a: A): S[A] =
    alg.value(a)
  override def empty[A]: F2[A] =
    map.to(alg.empty)
  override def cons[A](head: S[A], tail: F2[A]): F2[A] =
    map.to(alg.cons(head, map.from(tail)))
  override def flatMap[A, B](fa: F2[A])(f: FMap[A, B]): F2[B] =
    map.to(alg.flatMap(map.from(fa))(f))
  override def fix[A](f: ~>[A, A]): F2[A] =
    map.to(alg.fix(f))
}

class TwoWaySMapLAng[F[_], S1[_], S2[_]](map: TwoWayMap[S1, S2], alg: ListAlgebra[F, S1]) extends ListAlgebra[F, S2] {
  override def value[A](a: A): S2[A] = map.to(alg.value(a))
  override def empty[A]: F[A] = alg.empty
  override def cons[A](head: S2[A], tail: F[A]): F[A] = alg.cons(map.from(head), tail)
  override def flatMap[A, B](fa: F[A])(f: FMap[A, B]): F[B] = alg.flatMap(fa)(f)
  override def fix[A](f: ~>[A, A]): F[A] = alg.fix(f)
}

object MapEmpty {
  sealed trait Annotation[F[_], A] {
    def observe[S[_]](alg: ListAlgebra[F, S]): F[A] = new Annotate(alg).from(this)
    def isEmpty: Boolean = this match {
      case Empty() => true
      case _ => false
    }
  }
  case class Empty[F[_], A]() extends Annotation[F, A]
  case class Unknown[F[_], A](fa: F[A]) extends Annotation[F, A]

  class Annotate[F[_], S[_]](alg: ListAlgebra[F, S]) extends TwoWayMap[F, Annotation[F, ?]] {
    override def to[A](r: F[A]): Annotation[F, A] = Unknown(r)
    override def from[A](t: Annotation[F, A]): F[A] = t match {
      case Empty() => alg.empty
      case Unknown(fa) => fa
    }
  }

  class Annotator[F[_], S[_]](alg: ListAlgebra[F, S])
      extends TwoWayMapLang[F, Annotation[F, ?], S](new Annotate(alg), alg) {
    override def empty[A]: Annotation[F, A] = Empty()
    override def flatMap[A, B](fa: Annotation[F, A])(f: FMap[A, B]): Annotation[F, B] =
      fa match {
        case Empty() => Empty()
        case _ if EmptyChecker.isEmpty(f) => Empty()
        case _ => super.flatMap(fa)(f)
      }
    override def fix[A](f: A ~> A): Annotation[F, A] =
      if (EmptyChecker.isEmpty(f)) Empty()
      else super.fix(f)
  }

  object EmptyChecker extends Annotator[ConstUnit, ConstUnit](UnitInterpreter) {
    def isEmpty[A, B](f: FMap[A, B]): Boolean =
      f.run(this)(Unknown[ConstUnit, A](())).isEmpty
    def isEmpty[A, B](f: ~>[A, B]): Boolean =
      f.run(this)(Unknown[ConstUnit, A](())).isEmpty
  }
}
