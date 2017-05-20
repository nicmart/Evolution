package paint.evolution

import paint.geometry.Geometry.Point
import paint.evolution.Numeric._
import paint.evolution.SemigroupEvolution._
import paint.evolution.Evolution._

/**
  * Created by Nicolò Martini on 15/05/2017.
  */
object PointEvolution {
    def cartesian(x: Evolution[Double], y: Evolution[Double]): Evolution[Point] =
        x.compose(y)(Point.apply)

    def polar(norm: Evolution[Double], angle: Evolution[Double]): Evolution[Point] =
        norm.compose(angle)(Point.polar)

    def radial(angle: Evolution[Double])(start: Point): Evolution[Point] =
        angle.scan(start)(_.rotate(_))

    def radial(angle: Evolution[Double])(point: Evolution[Point]): Evolution[Point] =
        point.perturbate(angle.map(a => (p: Point) => p.rotate(a)))

    def relative(centre: Evolution[Point], point: Evolution[Point]): Evolution[Point] =
        centre.compose(point)(_ + _)

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

    def inRectangle(bottomRight: Point, topLeft: Point = Point.zero): Evolution[Point] = {
        cartesian(
            ball(bottomRight.x - topLeft.x),
            ball(bottomRight.y - topLeft.y)
        )
    }

    def independentSpeed(speed: Evolution[Point]): Evolution[Point => Point] =
        speed.map(x => _)

    def boundedBrownian(radius: Double, doubleEvo: Evolution[Double]): Evolution[Point] = {
        val speed = cartesian(doubleEvo, doubleEvo)
        def predicate(position: Point, speed: Point): Boolean = position.norm() <= radius
        solveIntegral(independentSpeed(speed), predicate)(Point.zero)
    }
}
