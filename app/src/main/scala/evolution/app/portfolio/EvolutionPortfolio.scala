package evolution.app.portfolio

import paint.evolution.Evolution._
import paint.evolution.SemigroupEvolutions._
import paint.evolution.NumericEvolutions._
import paint.evolution.PointEvolutions.{uniformRadial, _}
import paint.evolution.Evolution
import paint.geometry.Geometry.Point
import cats.implicits._
import paint.geometry.Geometry.Point.pointGroup
import paint.evolution.motion.MotionEvolutions._
import paint.evolution.motion._
import paint.evolution.implicits._

/**
  * Created by Nicolò Martini on 17/05/2017.
  */
object EvolutionPortfolio {
    def curlyRing(canvasSize: Point): Evolution[Point] = {
        val start = canvasSize / 2

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

    def ringEvo(canvasSize: Point): Evolution[Point] = {
        centeredIn(canvasSize / 2) {
            //integrate(cartesian(complement(), complement()).map(_ * 0.5))(Point.zero)
            ring(200, normal.map( _ * 20))
        }
    }

    def brownianSpeedRing(canvasSize: Point): Evolution[Point] = {
        solveIndependent(canvasSize / 2)(
            ball2D(2),
            (p, _) => p.inRectangle(Point.zero, canvasSize)
        ).positional
    }

    def accelerationRing(canvasSize: Point): Evolution[Point] = {
        centeredIn(canvasSize / 2) {
            solve2Independent(Point.zero, Point.zero)(ring(0.00001)).positional
        }
    }

    def brownianInCanvas(canvasSize: Point): Evolution[Point] = {
        def predicate: FirstOrderPredicate[Point] =
            (pos, _) => pos.inRectangle(-canvasSize / 2, canvasSize / 2)

        centeredIn(canvasSize / 2) {
            solveIndependent(Point.zero)(ring(2), predicate).positional
        }
    }

    def brownianStartingOnRing(canvasSize: Point): Evolution[Point] = {
        centeredIn(canvasSize / 2) {
            ring(300).flatMap(p => solveIndependent(p)(ring(1)).take(1000)).positional
        }
    }

    def linesStartingFromAGrid(canvasSize: Point): Evolution[Point] = {
        val w = 25
        val h = 10

        println(Point.grid(Point.zero, canvasSize, w, h))

        list {
            Point.grid(Point.zero, canvasSize, w, h).map { point =>
                solve2Independent(point, Point.zero)(ball2D(0.06)).positional.take(200)
            }
        }.flatten[Point].infinite
    }

    def gridParallelEvo(canvasSize: Point): Evolution[Point] = {
        val w = 10
        val h = 10

        sequenceParallel {
            Point.grid(Point.zero, canvasSize, w, h).map { point =>
                solveIndependent(point)(ring(1)).positional
            }
        }.flattenList
    }

    def regularPolygonEvo(canvasSize: Point): Evolution[Point] = {
        val edges = 40
        centeredIn(canvasSize / 2)(regularPolygon(edges, 500)).parallel(
            solveIndependent(_)(ring(1)).positional,
            edges
        ).flattenList
    }

    def nonParallelRegularPolygonEvo(canvasSize: Point): Evolution[Point] = {
        val edges = 40
        centeredIn(canvasSize / 2)(regularPolygon(edges, 500))
            .parallel(
                point => solve2Independent(point, Point.zero)(ball2D(0.06)).positional,
                edges
            ).take(100).infinite.flattenList
    }

    def waves(canvasSize: Point): Evolution[Point] = {
        val k = 0.00005
        val friction = 0.0004
        val speed = 0.1
        val accelerationEq: (Point, Point) => Point = (x, v) => Point(0, -k * x.y) - v * friction
        val accelerationEvo: AccelerationEvolution[Point] = constant(accelerationEq)
        val accelerationEvo2: AccelerationEvolution[Point] = accelerationEvo.zipWith(cartesian(constant(0), ball(0.0025))) {
            (eq, noise: Point) => {
                (x: Point, v: Point) => {
                    val acc = eq(x, v)
                    acc + noise
                }
            }
        }

        def vibration(from: Point) = translate(
            uniformLinear(from, Point(speed, 0)),
            MotionEvolutions.solve2[Point](Point.zero, Point.zero)(accelerationEvo2).positional
        )

        sequenceParallel(
            Point.sequence(40, Point.zero, canvasSize.copy(x = 0)).map(vibration)
        ).flattenList
    }

    def boundedBrownianEvo(canvasSize: Point): Evolution[Point] = {
        centeredIn(canvasSize / 2) {
            uniformRadial(Point(0, 300), 0.0001)
        }
    }

    def boundedBrownianLines(canvasSize: Point): Evolution[Point] = {
        def pointEvo(from: Point): Evolution[Point] =
            translate(
                uniformLinear(from, Point(0.02, 0)),
                boundedBrownian(25, ball(1))
            )

        sequence(
            Point.sequence(20, Point.zero, canvasSize.copy(x = 0)).map(pointEvo)
        ).flattenList
    }

    def slowedDownBrownian(canvasSize: Point): Evolution[Point] = {
        def pointEvo(from: Point): Evolution[Point] =
            translate(
                uniformLinear(from, Point(0.1, 0)),
                boundedBrownian(25, ball(1).slowDown(10))
            )

        sequence(
            Point.sequence(20, Point.zero, canvasSize.copy(x = 0)).map(pointEvo)
        ).flattenList
    }

    def randomlySlowedDownBrownian(canvasSize: Point): Evolution[Point] = {
        MotionEvolutions.solveIndependent(canvasSize / 2)(
            ball2D(1).slowDown(double.map[Int](d => if(d < 0.0001) 200 else 1)),
            (x, _) => x.inRectangle(Point.zero, canvasSize)
        ).positional
    }

    def randomPointEvo(canvasSize: Point): Evolution[Point] = {
        inRectangle(canvasSize).flatMap { point =>
            solveIndependent(point)(ring(2).slowDown(5)).positional.take(200)
        }
    }

    def drops(canvasSize: Point): Evolution[Point] = {
        val acc = Point(0, 0.003)
        val friction = 0.02
        val threshold = 0.1
        val randomForces = double.flatMap { p =>
            if (p < 0.01) ring(0.1).first else pure(Point.zero)
        }

        def accelerationEvolution: Evolution[AccelerationLaw[Point]] = randomForces map { randomAcc =>
            (position, velocity) =>
                if((randomAcc + velocity + acc).norm() < threshold) -velocity
                else randomAcc + acc - velocity * friction
        }

        def pointEvo(from: Point): Evolution[Point] = {
            MotionEvolutions.solve2(from, Point.zero)(accelerationEvolution).positional
        }

        val evo = sequenceParallel(
            Point.sequence(80, Point.zero, canvasSize.copy(y = 0)).map(pointEvo)
        ).flattenList

        evo
    }

    def randomAccWithFriction(canvasSize: Point): Evolution[Point] = {
        //val acc = 0.0004
        val acc = 0.001
        val friction = 0.0008
        def accelerationEvolution: Evolution[AccelerationLaw[Point]] = ball2D(acc) map { randomAcc =>
            (position, velocity) =>
                randomAcc - velocity * friction
        }

        MotionEvolutions.solve2(canvasSize / 2, Point.zero)(
            accelerationEvolution,
            (x, v, a) =>
                Point.distanceFromRectangle(x + v, Point.zero, canvasSize)
                    <= Point.distanceFromRectangle(x + v -a, Point.zero, canvasSize)
        ).positional
    }

    def brownianStraight(canvasSize: Point): Evolution[Point] = {
        rotate(
            canvasSize / 2,
            0,
            MotionEvolutions.solveIndependent(canvasSize / 2)(
                choice(Point.regularPolygon(4)).zipWith(choice(IndexedSeq(1, 2, 3, 4))) { (point, k) =>
                    point * k
                }.slowDown(10),
                (p, _) => true// p.inRectangle(Point.zero, canvasSize)
            ).positional
        )
    }

    def duplication(canvasSize: Point): Evolution[Point] = {
        val acc = 0.001
        val friction = 0.0004
        def accelerationEvolution: Evolution[AccelerationLaw[Point]] = ball2D(acc) map { randomAcc =>
            (position, velocity) =>
                randomAcc - velocity * friction
        }

        def pointEvo(from: Point): Evolution[Point] = {
            MotionEvolutions.solve2(from, Point.zero)(accelerationEvolution).positional
        }

        constant(List(canvasSize/2)).flatMapNextEvery(
            10000,
            (points, _) => {
                val newPoints = points.sortBy( point => (canvasSize / 2 - point).norm() ).take(points.length / 10 + 1)
                sequence((newPoints ::: points).map(pointEvo))
            }
        ).flattenList
    }

    def singlePoint(canvasSize: Point): Evolution[Point] = {
        constant(canvasSize/2)
    }

    def tinySegments(canvasSize: Point): Evolution[Point] = {
        //val acc = 0.0004
        val acc = 0.1
        val friction = 0.00001
        def accelerationEvolution: Evolution[AccelerationLaw[Point]] = ball2D(acc) map { randomAcc =>
            (position, velocity) =>
                randomAcc - velocity * friction
        }

        val k = 100

        MotionEvolutions.solve2((canvasSize / 2).copy(x = 0), Point(3, 0))(
            accelerationEvolution
        ).flatMap { case (position, velocity) =>
            val rotatedVel = velocity.rotate(Math.PI / 2)
            segment(position - rotatedVel * k, position + rotatedVel * k, 1)
        }
    }

    def current(canvasSize: Point): Evolution[Point] = {
        curlyRing(canvasSize)
        //brownianSpeedRing(canvasSize)
        //drops(canvasSize)
        //singlePoint(canvasSize)
        //randomAccWithFriction(canvasSize)
        //duplication(canvasSize)
//        ringEvo(canvasSize)
//        brownianSpeedRing(canvasSize)
//        accelerationRing(canvasSize)
//        brownianInCanvas(canvasSize)
//        brownianStartingOnRing(canvasSize)
//        linesStartingFromAGrid(canvasSize)
//        gridParallelEvo(canvasSize)
//        regularPolygonEvo(canvasSize)
//        nonParallelRegularPolygonEvo(canvasSize)
//        waves(canvasSize)
//        boundedBrownianEvo(canvasSize)
//        duplication(canvasSize)
//        randomAccWithFriction(canvasSize)
//        tinySegments(canvasSize)
//        randomPointEvo(canvasSize)
    }
}
