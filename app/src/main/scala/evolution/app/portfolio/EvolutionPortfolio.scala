package evolution.app.portfolio

import paint.evolution.Evolution._
import paint.evolution.SemigroupEvolution._
import paint.evolution.Numeric._
import paint.evolution.PointEvolution.{uniformRadial, _}
import paint.evolution.Evolution
import paint.geometry.Geometry.Point
import cats.implicits._
import paint.geometry.Geometry.Point.pointMonoid

/**
  * Created by Nicolò Martini on 17/05/2017.
  */
object EvolutionPortfolio {
    def brownian: Evolution[Point] = {
        val start = Point(1000, 500)
        //integrate(cartesian(ball(1), ball(1)))(DoublePoint(500, 500))

        //integrate(cartesian(pure(0.01), ball(2)))(DoublePoint(0, 500))

        integrate(polar(pure(1), ball(2 * Math.PI/ 2.02)))(Point(500, 500))

        integrateMulti(
            cartesian(ball(1), ball(1))
        )(
            List(Point(1000, 500),
                Point(0.001, 0))
        )

        centeredIn(start) {
            translate(
                translate(
                    uniformRadial(Point(0, 300), 0.0001),
                    translate(
                        uniformRadial(Point(0, 30), 0.003),
                        uniformRadial(Point(0, 5), 0.01)
                    )
                ),
                cartesian(ball(5), ball(5)).slowDown(50)
            )
        }

        //cartesian(ball(5), ball(5))
    }

    def list: Evolution[List[Point]] = {
        centeredIn(Point(1000, 500)) {
            integrate(cartesian(ball(1), ball(1)))(Point.zero)
        }.map(List(_))
    }

    def ringEvo: Evolution[List[Point]] = {
        centeredIn(Point(1000, 500)) {
            //integrate(cartesian(complement(), complement()).map(_ * 0.5))(Point.zero)
            ring(200, normal.map( _ * 20))
        }.map(List(_))
    }

    def brownianSpeedRing: Evolution[List[Point]] = {
        centeredIn(Point(1000, 500)) {
            integrate(ring(2))(Point.zero)
        }.map(List(_))
    }

    def accelerationRing: Evolution[List[Point]] = {
        centeredIn(Point(1000, 500)) {
            integrateMulti(ring(0.00001))(List(Point.zero, Point.zero))
        }.map(List(_))
    }

    def integrateCond(canvasSize: Point): Evolution[List[Point]] = {
        centeredIn(canvasSize / 2) {
            integrateConditional(ring(2))(Point.zero)(_.inRectangle(-canvasSize / 2, canvasSize / 2))
        }.map(List(_))
    }

    def randomRing(canvasSize: Point): Evolution[List[Point]] = {
        centeredIn(canvasSize / 2) {
            ring(300).replaceEvery(1000, integrate(ring(1)))
        }.map(List(_))
    }

    def gridEvo(canvasSize: Point): Evolution[List[Point]] = {
        val w = 25
        val h = 10
        grid(canvasSize.x, canvasSize.y, w, h)
            .replaceEvery(500, integrate(ball2D(2)))
            .map(List(_))
    }

    def current(canvasSize: Point): Evolution[List[Point]] = {
        gridEvo(canvasSize)
        //brownian.map(List(_))
    }
}
