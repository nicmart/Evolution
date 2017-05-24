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
        x.map2(y)(Point.apply)

    def polar(norm: Evolution[Double], angle: Evolution[Double]): Evolution[Point] =
        norm.map2(angle)(Point.polar)

    def radial(angle: Evolution[Double])(start: Point): Evolution[Point] =
        angle.scan(start)(_.rotate(_))

    def radial(angle: Evolution[Double])(point: Evolution[Point]): Evolution[Point] =
        point.perturbate(angle.map(a => (p: Point) => p.rotate(a)))

    def relative(centre: Evolution[Point], point: Evolution[Point]): Evolution[Point] =
        centre.map2(point)(_ + _)

    def uniformLinear(from: Point, speed: Point): Evolution[Point] =
        integrate(pure(speed))(from)

    def uniformRadial(from: Point, radialSpeed: Double): Evolution[Point] =
        transition(from)(_.rotate(radialSpeed))

    def centeredIn(center: Point)(ev: Evolution[Point]): Evolution[Point] =
        ev.map(p => p + center)

    def complementOf(ev: Evolution[Point], radius: Double): Evolution[Point] =
        ev map { point =>
            point.versor().getOrElse(Point.zero) * radius - point
        }

    def translateRadial(ev: Evolution[Point], radius: Double): Evolution[Point] =
        ev map { point =>
            point.versor().getOrElse(Point.zero) * radius + point
        }

    def ball2D(radius: Double): Evolution[Point] =
        cartesian(ball(radius), ball(radius))

    def ring(radius: Double, size: Evolution[Double]): Evolution[Point] =
        polar(
            size.map(_ + radius),
            double.map(_ * 2 * Math.PI)
        )

    def ring(radius: Double): Evolution[Point] =
        polar(
            pure(radius),
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

    def independentSpeed(speed: Evolution[Point]): Evolution[Point => Point] =
        speed.map(x => _)

    def brownian(pointEvo: Evolution[Point]): Evolution[Point] = {
        MotionEvolutions.solveIndependent(Point.zero)(pointEvo).positional
    }

    def boundedBrownian(radius: Double, doubleEvo: Evolution[Double]): Evolution[Point] = {
        val speed = cartesian(doubleEvo, doubleEvo)
        def predicate(position: Point, speed: Point): Boolean = position.norm() <= radius
        MotionEvolutions.solveIndependent(Point.zero)(speed, predicate).positional
    }
}
