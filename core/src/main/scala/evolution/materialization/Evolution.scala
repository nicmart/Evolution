package evolution.materialization
import evolution.geometry.Point
import evolution.rng.PerlinNoise

import scala.collection.AbstractIterator
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

sealed trait Evolution2[+T] {
  def run: Iterator[T]
}
