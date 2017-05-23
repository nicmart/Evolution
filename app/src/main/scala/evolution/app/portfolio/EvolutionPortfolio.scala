package evolution.app.portfolio

import paint.evolution.Evolution._
import paint.evolution.SemigroupEvolutions._
import paint.evolution.NumericEvolutions._
import paint.evolution.PointEvolutions.{uniformRadial, _}
import paint.evolution.Evolution
import paint.geometry.Geometry.Point
import cats.implicits._
import paint.geometry.Geometry.Point.pointMonoid
import paint.evolution.motion._

/**
  * Created by Nicolò Martini on 17/05/2017.
  */
object EvolutionPortfolio {
    def brownianEvo: Evolution[Point] = {
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

    def brownianSpeedRing(canvasSize: Point): Evolution[Point] = {
        MotionEvolutions.solveIndependent(canvasSize / 2)(
            ball2D(2),
            (p, _) => p.inRectangle(Point.zero, canvasSize)
        )
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
            .replaceEvery(100, point => integrateMulti(ball2D(0.06))(List(point, Point.zero)))
            .map(List(_))
    }

    def gridParallelEvo(canvasSize: Point): Evolution[List[Point]] = {
        val w = 50
        val h = 20
        grid(canvasSize.x, canvasSize.y, w, h)
            .parallel(integrate(ring(1)), (w + 1) * (h + 1))
    }

    def regularPolygonEvo(canvasSize: Point): Evolution[List[Point]] = {
        val edges = 40
        centeredIn(canvasSize / 2)(regularPolygon(edges, 500)).parallel(
            integrate(ring(1)),
            edges
        )
    }

    def nonParallelRegularPolygonEvo(canvasSize: Point): Evolution[List[Point]] = {
        val edges = 40
        centeredIn(canvasSize / 2)(regularPolygon(edges, 500))
            .parallel(
                point => integrateMulti(ball2D(0.06))(List(point, Point.zero)),
                edges
            ).restartEvery(100)
    }

    def solveIntegralEvo(canvasSize: Point): Evolution[Point] = {
        val accelerationEq: (Point, Point) => Point = (x, v) => Point(0, -0.00005 * x.y) - v * 0.0004
        val accelerationEvo: AccelerationEvolution[Point] = pure(accelerationEq)
        val accelerationEvo2: AccelerationEvolution[Point] = accelerationEvo.map2(cartesian(pure(0), ball(0.0025))) {
            (eq, noise: Point) => {
                (x: Point, v: Point) => {
                    val acc = eq(x, v)
                    acc + noise
                }
            }
        }

        def vibration(from: Point) = translate(
            uniformLinear(from, Point(0.1, 0)),
            MotionEvolutions.solve2[Point](Point.zero, Point.zero)(accelerationEvo2)
        )

        flatten(sequence(
            Point.sequence(40, Point.zero, canvasSize.copy(x = 0)).map(vibration)
        ))
    }

    def boundedBrownianEvo(canvasSize: Point): Evolution[List[Point]] = {
        centeredIn(canvasSize / 2) {
            translate(
                uniformRadial(Point(0, 300), 0.0001),
                boundedBrownian(15, ball(1))
            )
        }.map(List(_))
    }

    def boundedBrownianLines(canvasSize: Point): Evolution[Point] = {
        def pointEvo(from: Point): Evolution[Point] =
            translate(
                uniformLinear(from, Point(0.02, 0)),
                boundedBrownian(25, ball(1))
            )

        flatten(sequence(
            Point.sequence(20, Point.zero, canvasSize.copy(x = 0)).map(pointEvo)
        ))
    }

    def slowedDownBrownian(canvasSize: Point): Evolution[Point] = {
        def pointEvo(from: Point): Evolution[Point] =
            translate(
                uniformLinear(from, Point(0.1, 0)),
                boundedBrownian(25, ball(1).slowDown(10))
            )

        flatten(sequence(
            Point.sequence(20, Point.zero, canvasSize.copy(x = 0)).map(pointEvo)
        ))
    }

    def randomlySlowedDownBrownian(canvasSize: Point): Evolution[Point] = {
        MotionEvolutions.solveIndependent(canvasSize / 2)(
            ball2D(1).slowDown(double.map[Int](d => if(d < 0.0001) 200 else 1)),
            (x, _) => x.inRectangle(Point.zero, canvasSize)
        )
    }

    def randomPointEvo(canvasSize: Point): Evolution[Point] = {
        inRectangle(canvasSize)
            .replaceEvery[Point](200, point => solve(independentSpeed(ring(2).slowDown(5)))(point))
    }

    def drops(canvasSize: Point): Evolution[Point] = {
        val acc = Point(0, 0.003)
        val friction = 0.02
        val threshold = 0.1
        val randomForces = double.flatMap { p =>
            if (p < 0.01) ring(0.1) else pure(Point.zero)
        }

        def accelerationEvolution: Evolution[AccelerationLaw[Point]] = randomForces map { randomAcc =>
            (position, velocity) =>
                if((randomAcc + velocity + acc).norm() < threshold) -velocity
                else randomAcc + acc - velocity * friction
        }

//        pure { (position, velocity) =>
//            Point(0, acc) - velocity * friction
//        }

        def pointEvo(from: Point): Evolution[Point] = {
            MotionEvolutions.solve2(from, Point.zero)(accelerationEvolution)
        }

        flatten(sequence(
            Point.sequence(80, Point.zero, canvasSize.copy(y = 0)).map(pointEvo)
        ))
    }

    def randomAccWithFriction(canvasSize: Point): Evolution[Point] = {
        //val acc = 0.0004
        val acc = 0.0001
        val friction = 0.00008
        def accelerationEvolution: Evolution[AccelerationLaw[Point]] = ball2D(acc) map { randomAcc =>
            (position, velocity) =>
                randomAcc - velocity * friction
        }

        MotionEvolutions.solve2(canvasSize / 2, Point.zero)(
            accelerationEvolution,
            (x, v, a) =>
                Point.distanceFromRectangle(x + v, Point.zero, canvasSize)
                    <= Point.distanceFromRectangle(x + v -a, Point.zero, canvasSize)
        )
    }

    def brownianStraight(canvasSize: Point): Evolution[Point] = {
        rotate(
            canvasSize / 2,
            0,
            MotionEvolutions.solveIndependent(canvasSize / 2)(
                choice(Point.regularPolygon(4)).map2(choice(IndexedSeq(1, 2, 3, 4))) { (point, k) =>
                    point * k
                }.slowDown(10),
                (p, _) => true// p.inRectangle(Point.zero, canvasSize)
            )
        )
    }

    def duplication(canvasSize: Point): Evolution[Point] = {
        flatten(
            MotionEvolutions.solveIndependent(canvasSize/2)(ball2D(1)).map(List(_))
                .flatMapNextEvery(
                    100000,
                    (points, pointsEvo) => {
                        println(points.length)
                        pointsEvo.map2(pointsEvo)(_ ::: _)
                    }
                )
        )
    }

    def current(canvasSize: Point): Evolution[Point] = {
        //brownianSpeedRing(canvasSize)
        //randomAccWithFriction(canvasSize)
        duplication(canvasSize)
    }
}
