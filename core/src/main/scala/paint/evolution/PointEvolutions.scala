package paint.evolution

import paint.geometry.Geometry.Point
import paint.evolution.NumericEvolutions._
import paint.evolution.SemigroupEvolutions._
import paint.evolution.Evolution._
import paint.evolution.motion.MotionEvolutions
import paint.evolution.implicits._

/**
  * Created by Nicolò Martini on 15/05/2017.
  */
object PointEvolutions {
    def cartesian(x: Evolution[Double], y: Evolution[Double]): Evolution[Point] =
        x.zipWith(y)(Point.apply)

    def polar(norm: Evolution[Double], angle: Evolution[Double]): Evolution[Point] =
        norm.zipWith(angle)(Point.polar)

    def uniformLinear(from: Point, speed: Point): Evolution[Point] =
        MotionEvolutions.solveIndependent(from)(constant(speed)).positional

    def uniformRadial(from: Point, radialSpeed: Double): Evolution[Point] =
        MotionEvolutions.solve0Static(from)(_.rotate(radialSpeed)).positional

    def centeredIn(center: Point)(ev: Evolution[Point]): Evolution[Point] =
        ev.map(p => p + center)

    def rectangle2D(radius: Double): Evolution[Point] =
        cartesian(ball(radius), ball(radius))

    def ball2D(radius: Double): Evolution[Point] =
        polar(doubleRange(0, radius), double.map(_ * 2 * Math.PI))

    def segment(p1: Point, p2: Point, speed: Double, lambda: Double = 0): Evolution[Point] = {
        val norm = (p2 - p1).norm()
        if (lambda > 1) empty
        else {
            val nextLambda = lambda + speed / norm
            pure(p1 * lambda + p2 * (1 - lambda)).append(segment(p1, p2, speed, nextLambda))
        }
    }

    def ring(radius: Double, size: Evolution[Double]): Evolution[Point] =
        polar(
            size.map(_ + radius),
            double.map(_ * 2 * Math.PI)
        )

    def ring(radius: Double): Evolution[Point] =
        polar(
            constant(radius),
            double.map(_ * 2 * Math.PI)
        )

    def regularPolygon(edges: Int, radius: Double = 1): Evolution[Point] = {
        val start = Point(0, radius)
        val points = (0 until edges).map( 2 * Math.PI * _ / edges).map(start.rotate).toList
        cycle(points)
    }

    def rotate(center: Point, angle: Double, ev: Evolution[Point]): Evolution[Point] =
        ev.map(p => (p - center).rotate(angle) + center)

    def evoRotate(center: Evolution[Point], angle: Evolution[Double])(ev: Evolution[Point]): Evolution[Point] = {
        center.zip(angle).zipWith(ev) { case ((cc, ang), point) =>
            (point - cc).rotate(ang) + cc
        }
    }

    def inRectangle(bottomRight: Point, topLeft: Point = Point.zero): Evolution[Point] = {
        cartesian(
            double.map(_ * (bottomRight.x - topLeft.x)),
            double.map(_ * (bottomRight.y - topLeft.y))
        )
    }

    def boundedBrownian(radius: Double, doubleEvo: Evolution[Double]): Evolution[Point] = {
        val speed = cartesian(doubleEvo, doubleEvo)
        def predicate(position: Point, speed: Point): Boolean = position.norm() <= radius
        MotionEvolutions.solveIndependent(Point.zero)(speed, predicate).positional
    }

    /**
      * To test performance
      */
    def sum(ev1: Evolution[Point], ev2: Evolution[Point]): Evolution[Point] =
        Evolution { rng =>
            val (rng2, optEv1Next) = ev1.run(rng)
            val (rng3, optEv2Next) = ev2.run(rng2)
            (optEv1Next, optEv2Next) match {
                case (Some((p1, ev1Next)), Some((p2, ev2Next))) => (rng3, Some(p1 + p2, sum(ev1Next, ev2Next)))
                case _ => (rng3, None)
            }
        }
}
