package evolution.data

import shapeless._

object ByTypeMap {
  implicit def valueInHead[H, T <: HList]: HasValue[H :: T, H] =
    new HasValue[H :: T, H] {
      override def get(t: H :: T): H = t.head
      override def set(t: H :: T, v: H): H :: T = v :: t.tail
    }

  implicit def valueInTail[H, T <: HList, V](implicit tailHasValue: HasValue[T, V]): HasValue[H :: T, V] =
    new HasValue[H :: T, V] {
      override def get(x: H :: T): V = tailHasValue.get(x.tail)
      override def set(x: H :: T, v: V): H :: T = x.head :: tailHasValue.set(x.tail, v)
    }

  implicit def hasValueOps[T <: HList](t: T): HasValueOps[T] = HasValueOps(t)
  implicit def byMapOps[T <: HList](t: T): ByTypeMapOps[T] = ByTypeMapOps(t)

  case class ByTypeMapOps[X <: HList](x: X) extends AnyVal {
    def add[H](h: H): H :: X = h :: x
  }

  def get[V] = new GetterHelper[V]
  def set[V] = new SetterHelper[V]

  class GetterHelper[V] {
    def apply[X](x: X)(implicit hasValue: HasValue[X, V]): V = hasValue.get(x)
  }

  class SetterHelper[V] {
    def apply[X](x: X, v: V)(implicit hasValue: HasValue[X, V]): X = hasValue.set(x, v)
  }
}

trait HasValue[X, V] {
  def get(x: X): V
  def set(x: X, v: V): X
}

object HasValue {
  def instance[X, V](_get: X => V, _set: (X, V) => X): HasValue[X, V] = new HasValue[X, V] {
    override def get(x: X): V = _get(x)
    override def set(x: X, v: V): X = _set(x, v)
  }

//  implicit def isTransitive[A, B, V](implicit aHasB: HasValue[A, B], bHasV: HasValue[B, V]): HasValue[A, V] =
//    instance(a => bHasV.get(aHasB.get(a)), (a, v) => aHasB.set(a, bHasV.set(aHasB.get(a), v)))
}

case class HasValueOps[X](x: X) extends AnyVal {
  def get[V](implicit hasValue: HasValue[X, V]): V = hasValue.get(x)
  def set[V](v: V)(implicit hasValue: HasValue[X, V]): X = hasValue.set(x, v)
}
