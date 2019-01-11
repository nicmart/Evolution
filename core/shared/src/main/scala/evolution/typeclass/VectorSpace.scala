package evolution.typeclass
import cats.kernel.Monoid
import cats.instances.double._
import cats.instances.int._

trait VectorSpace[T] {
  def monoid: Monoid[T]
  def mult(k: Double, t: T): T
  def add(t1: T, t2: T): T = monoid.combine(t1, t2)
}

object VectorSpace {
  def apply[T](implicit instance: VectorSpace[T]): VectorSpace[T] = instance

  implicit val doubleInstance: VectorSpace[Double] = new VectorSpace[Double] {
    override def monoid: Monoid[Double] = Monoid[Double]
    override def mult(k: Double, t: Double): Double = k * t
  }

  // This does not make any sense, Integers are not a VS over the reals
  implicit val intInstance: VectorSpace[Int] = new VectorSpace[Int] {
    override def monoid: Monoid[Int] = Monoid[Int]
    override def mult(k: Double, t: Int): Int = (k * t).toInt
  }

  implicit def monoidInstance[T](implicit vs: VectorSpace[T]): Monoid[T] = vs.monoid
}
