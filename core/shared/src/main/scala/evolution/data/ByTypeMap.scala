package evolution.data

import evolution.data.ByTypeMap.With

sealed trait ByTypeMap

object ByTypeMap {
  case object Empty extends ByTypeMap
  final case class With[H, T <: ByTypeMap](head: H, tail: T) extends ByTypeMap

  implicit def valueInHead[H, T <: ByTypeMap]: HasValue[With[H, T], H] =
    new HasValue[With[H, T], H] {
      override def get(t: With[H, T]): H = t.head
      override def set(t: With[H, T], v: H): With[H, T] = With(v, t.tail)
    }

  implicit def valueInTail[H, T <: ByTypeMap, V](implicit tailHasValue: HasValue[T, V]): HasValue[With[H, T], V] =
    new HasValue[With[H, T], V] {
      override def get(x: With[H, T]): V = tailHasValue.get(x.tail)
      override def set(x: With[H, T], v: V): With[H, T] = With(x.head, tailHasValue.set(x.tail, v))
    }

  implicit def hasValueOps[T <: ByTypeMap](t: T): HasValueOps[T] = HasValueOps(t)
  implicit def byMapOps[T <: ByTypeMap](t: T): ByTypeMapOps[T] = ByTypeMapOps(t)

  case class ByTypeMapOps[X <: ByTypeMap](x: X) extends AnyVal {
    def add[H](h: H): With[H, X] = With(h, x)
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

case class HasValueOps[X](x: X) extends AnyVal {
  def get[V](implicit hasValue: HasValue[X, V]): V = hasValue.get(x)
  def set[V](v: V)(implicit hasValue: HasValue[X, V]): X = hasValue.set(x, v)
}
