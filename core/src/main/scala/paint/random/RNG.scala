package paint.random

import scala.util.Random

/**
  * Created by nic on 06/12/2016.
  */
trait RNG {
    def nextInt: (Int, RNG)
    def nextRNG: RNG = nextInt._2
}

case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }
}

case class SequenceRNG(from: Int) extends RNG {
    override def nextInt: (Int, RNG) = (from, SequenceRNG(from + 1))
}