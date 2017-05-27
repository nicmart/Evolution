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

    def radial(angle: Evolution[Double])(start: Point): Evolution[Point] =
        angle.scan(start)(_.rotate(_))

    def radial(angle: Evolution[Double])(point: Evolution[Point]): Evolution[Point] =
        point.perturbate(angle.map(a => (p: Point) => p.rotate(a)))

    def relative(centre: Evolution[Point], point: Evolution[Point]): Evolution[Point] =
        centre.zipWith(point)(_ + _)

    def uniformLinear(from: Point, speed: Point): Evolution[Point] =
        MotionEvolutions.solveIndependent(from)(constant(speed)).positional

    def uniformRadial(from: Point, radialSpeed: Double): Evolution[Point] =
        MotionEvolutions.solveStatic(from)(_.rotate(radialSpeed)).positional

    def centeredIn(center: Point)(ev: Evolution[Point]): Evolution[Point] =
        ev.map(p => p + center)

    def ball2D(radius: Double): Evolution[Point] =
        cartesian(ball(radius), ball(radius))

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

    def grid(w: Double, h: Double, x: Int, y: Int): Evolution[Point] = {
        val xEv = cycle((0 to x).toList).map(w * _ / x)
        val yEv = cycle((0 to y).toList).map(h * _ / y)
        yEv.replaceEvery[Point](x + 1, y => xEv.map(Point(_, y)))
    }

    def rotate(center: Point, angle: Double, ev: Evolution[Point]): Evolution[Point] =
        ev.map(p => (p - center).rotate(angle) + center)

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
}
