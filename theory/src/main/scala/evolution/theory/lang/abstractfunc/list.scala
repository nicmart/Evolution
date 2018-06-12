package evolution.theory.lang.abstractfunc

import evolution.theory.lang.abstractfunc.Types._
import evolution.theory.lang.patternmatching.Const.{Annotation, Constant, Unknown}
import evolution.theory.lang.patternmatching.{Lang, TwoWayMap}

trait ListAlgebra[F[_], S[_]] {
  def value[A](a: A): S[A]
  def empty[A]: F[A]
  def cons[A](head: S[A], tail: F[A]): F[A]
  def mapEmpty[A](fa: F[A])(fa2: F[A]): F[A]
  def mapCons[A, B](fa: F[A])(f: SFMap[A, B]): F[B]
  def flatMap[A, B](fa: F[A])(f: FMap[A, B]): F[B]
  def fix[A](f: A ~> A): F[A]
  def fix2[A, B, C](f: Map2[A, B, C]): F[A] => F[B] => F[C] = ???
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
  def concat[A]: F[A] => F[A] => F[A] =
    fix2(new Map2[A, A, A] {
      override def run[F2[_], S2[_]](alg2: ListAlgebra[F2, S2]): (F2[A] => F2[A] => F2[A]) => F2[A] => F2[A] => F2[A] =
        concatRec =>
          fa1 =>
            fa2 =>
              alg2.mapCons(alg2.mapEmpty(fa1)(fa2))(new SFMap[A, A] {
                override def run[F3[_], S3[_]](alg3: ListAlgebra[F3, S3]): (S3[A], F3[A]) => F3[A] =
                  //MM, here I need an F3... but what I have us concatRec
                  (s3, f3) => alg3.cons(s3, ???)
              })
    })

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

trait Map1[A, B] {
  def run[F[_], S[_]](alg: ListAlgebra[F, S]): (F[A] => F[B]) => (F[A] => F[B])
}

trait Map2[A, B, C] {
  def run[F[_], S[_]](alg: ListAlgebra[F, S]): (F[A] => F[B] => F[C]) => (F[A] => F[B] => F[C])
}

object UnitInterpreter extends ListAlgebra[ConstUnit, ConstUnit] {
  override def value[A](a: A): Unit = ()
  override def empty[A]: Unit = ()
  override def cons[A](head: Unit, tail: Unit): Unit = ()
  override def flatMap[A, B](fa: Unit)(f: FMap[A, B]): Unit = ()
  override def fix[A](f: ~>[A, A]): Unit = ()
  override def mapEmpty[A](fa: Unit)(fa2: Unit): Unit = ()
  override def mapCons[A, B](fa: Unit)(f: SFMap[A, B]): Unit = ()
}

object LazyStreamInterpreter extends ListAlgebra[LazyStream, Id] {
  override def empty[A]: LazyStream[A] = () => Stream.empty
  override def cons[A](head: A, tail: LazyStream[A]): LazyStream[A] = () => head #:: tail()
  override def fix[A](f: A ~> A): LazyStream[A] = f.run(this)(() => fix(f)())
  override def value[A](a: A): Id[A] = a
  override def flatMap[A, B](fa: LazyStream[A])(f: FMap[A, B]): LazyStream[B] =
    () => fa().flatMap(a => f.run(this)(a)())
  override def mapEmpty[A](fa: LazyStream[A])(fa2: LazyStream[A]): LazyStream[A] =
    () => {
      val fas = fa()
      if (fas.nonEmpty) fas else fa2()
    }
  override def mapCons[A, B](fa: LazyStream[A])(f: SFMap[A, B]): LazyStream[B] =
    () => {
      val fas = fa()
      fas match {
        case Stream.Empty => Stream.Empty
        case head #:: tail => f.run(this)(head, () => tail)()
      }
    }
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
  override def value[A](a: A): String = a.toString
  override def flatMap[A, B](fa: String)(f: FMap[A, B]): String =
    s"flatMap($fa)(x => ${f.run(this)("x")})"
  override def mapEmpty[A](fa: String)(fa2: String): String =
    s"mapEmpty($fa)($fa2)"
  override def mapCons[A, B](fa: String)(f: SFMap[A, B]): String =
    s"mapCons($fa)((h, t) => ${f.run(this)("h", "t")})"
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
  override def mapEmpty[A](fa: F2[A])(fa2: F2[A]): F2[A] =
    map.to(alg.mapEmpty(map.from(fa))(map.from(fa2)))
  override def mapCons[A, B](fa: F2[A])(f: SFMap[A, B]): F2[B] =
    map.to(alg.mapCons(map.from(fa))(f))
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
    override def mapEmpty[A](fa: Annotation[F, A])(fa2: Annotation[F, A]): Annotation[F, A] =
      if (fa.isEmpty && fa2.isEmpty) Empty() else super.mapEmpty(fa)(fa2)
    override def mapCons[A, B](fa: Annotation[F, A])(f: SFMap[A, B]): Annotation[F, B] =
      fa match {
        case Empty() => Empty()
        case _ if EmptyChecker.isEmpty(f) => Empty()
        case _ => super.mapCons(fa)(f)
      }
  }

  object EmptyChecker extends Annotator[ConstUnit, ConstUnit](UnitInterpreter) {
    def isEmpty[A, B](f: FMap[A, B]): Boolean =
      f.run(this)(Unknown[ConstUnit, A](())).isEmpty
    def isEmpty[A, B](f: ~>[A, B]): Boolean =
      f.run(this)(Unknown[ConstUnit, A](())).isEmpty
    def isEmpty[A, B](f: SFMap[A, B]): Boolean =
      f.run(this)((), Unknown[ConstUnit, A](())).isEmpty
  }
}
