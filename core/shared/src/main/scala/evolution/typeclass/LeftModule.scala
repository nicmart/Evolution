package evolution.typeclass

import evolution.geometry.Point

trait LeftModule[K, T] {
  def multiply(k: K, t: T): T
}

object LeftModule {
  def apply[K, T](implicit leftModule: LeftModule[K, T]): LeftModule[K, T] = leftModule

  implicit val dblDbl: LeftModule[Double, Double] = (k, t) => k * t
  implicit val dblPoint: LeftModule[Double, Point] = (k, t) => t * k
  implicit val intInt: LeftModule[Int, Int] = (k, t) => k * t
  implicit val intDbl: LeftModule[Int, Double] = (k, t) => k * t
  implicit val intPoint: LeftModule[Int, Point] = (k, t) => t * k
}
