package paint.evolution

import paint.geometry.Geometry.DoublePoint

/**
  * Created by Nicolò Martini on 15/05/2017.
  */
object Point {
    def cartesian(x: Evolution[Double], y: Evolution[Double]): Evolution[DoublePoint] =
        x.compose(y)(DoublePoint.apply)

    def polar(norm: Evolution[Double], angle: Evolution[Double]): Evolution[DoublePoint] =
        norm.compose(angle)(DoublePoint.polar)

    def radial(angle: Evolution[Double])(start: DoublePoint): Evolution[DoublePoint] =
        angle.scan(start)(_.rotate(_))

    def radial(angle: Evolution[Double])(point: Evolution[DoublePoint]): Evolution[DoublePoint] =
        point.perturbate(angle.map(a => (p: DoublePoint) => p.rotate(a)))

    def relative(centre: Evolution[DoublePoint], point: Evolution[DoublePoint]): Evolution[DoublePoint] =
        centre.compose(point)(_ + _)
}
