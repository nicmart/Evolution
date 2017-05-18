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
}
