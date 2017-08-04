package evolution.app.portfolio

import paint.evolution.Evolution._
import paint.evolution.SemigroupEvolutions._
import paint.evolution.NumericEvolutions._
import paint.evolution.PointEvolutions.{uniformRadial, _}
import paint.evolution.Evolution
import paint.geometry.Geometry.Point
import cats.implicits._
import evolution.app.canvas.CanvasSize
import evolution.app.model.{Drawing, DrawingList}
import paint.geometry.Geometry.Point.pointGroup
import paint.evolution.motion.MotionEvolutions._
import paint.evolution.motion._
import paint.evolution.implicits._

/**
  * Created by Nicolò Martini on 17/05/2017.
  */
object EvolutionPortfolio {
    def curlyRing(canvasSize: CanvasSize): Evolution[Point] = {
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

    def ringEvo(canvasSize: CanvasSize): Evolution[Point] = {
        centeredIn(canvasSize / 2) {
            //integrate(cartesian(complement(), complement()).map(_ * 0.5))(Point.zero)
            ring(200, normal.map( _ * 20))
        }
    }

    def brownianSpeedRing(canvasSize: CanvasSize): Evolution[Point] = {
        solveIndependent(canvasSize / 2)(
            ball2D(2),
            (p, _) => p.inRectangle(Point.zero, canvasSize)
        ).positional
    }

    def accelerationRing(canvasSize: CanvasSize): Evolution[Point] = {
        centeredIn(canvasSize / 2) {
            solve2Independent(Point.zero, Point.zero)(ring(0.00001)).positional
        }
    }

    def brownianInCanvas(canvasSize: CanvasSize): Evolution[Point] = {
        def predicate: FirstOrderPredicate[Point] =
            (pos, _) => pos.inRectangle(-canvasSize / 2, canvasSize / 2)

        centeredIn(canvasSize / 2) {
            solveIndependent(Point.zero)(ring(2), predicate).positional
        }
    }

    def brownianStartingOnRing(canvasSize: CanvasSize): Evolution[Point] = {
        centeredIn(canvasSize / 2) {
            ring(300).flatMap(p => solveIndependent(p)(ring(1)).take(1000)).positional
        }
    }

    def linesStartingFromAGrid(canvasSize: CanvasSize): Evolution[Point] = {
        val w = 25
        val h = 10

        println(Point.grid(Point.zero, canvasSize, w, h))

        list {
            Point.grid(Point.zero, canvasSize, w, h).map { point =>
                solve2Independent(point, Point.zero)(ball2D(0.06)).positional.take(200)
            }
        }.flatten[Point].infinite
    }

    def gridParallelEvo(canvasSize: CanvasSize): Evolution[Point] = {
        val w = 10
        val h = 10

        sequenceParallel {
            Point.grid(Point.zero, canvasSize, w, h).map { point =>
                solveIndependent(point)(ring(1)).positional
            }
        }.flattenList
    }

    def regularPolygonEvo(canvasSize: CanvasSize): Evolution[Point] = {
        val edges = 40
        centeredIn(canvasSize / 2)(regularPolygon(edges, 500)).parallel(
            solveIndependent(_)(ring(1)).positional,
            edges
        ).flattenList
    }

    def nonParallelRegularPolygonEvo(canvasSize: CanvasSize): Evolution[Point] = {
        val edges = 40
        centeredIn(canvasSize / 2)(regularPolygon(edges, 500))
            .parallel(
                point => solve2Independent(point, Point.zero)(ball2D(0.06)).positional,
                edges
            ).take(100).infinite.flattenList
    }

    def waves(canvasSize: CanvasSize): Evolution[Point] = {
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

    def boundedBrownianEvo(canvasSize: CanvasSize): Evolution[Point] = {
        centeredIn(canvasSize / 2) {
            uniformRadial(Point(0, 300), 0.0001)
        }
    }

    def boundedBrownianLines(canvasSize: CanvasSize): Evolution[Point] = {
        def pointEvo(from: Point): Evolution[Point] =
            translate(
                uniformLinear(from, Point(0.02, 0)),
                boundedBrownian(25, ball(1))
            )

        sequence(
            Point.sequence(20, Point.zero, canvasSize.copy(x = 0)).map(pointEvo)
        ).flattenList
    }

    def slowedDownBrownian(canvasSize: CanvasSize): Evolution[Point] = {
        def pointEvo(from: Point): Evolution[Point] =
            translate(
                uniformLinear(from, Point(0.1, 0)),
                boundedBrownian(25, ball(1).slowDown(10))
            )

        sequence(
            Point.sequence(20, Point.zero, canvasSize.copy(x = 0)).map(pointEvo)
        ).flattenList
    }

    def randomlySlowedDownBrownian(canvasSize: CanvasSize): Evolution[Point] = {
        MotionEvolutions.solveIndependent(canvasSize / 2)(
            ball2D(1).slowDown(double.map[Int](d => if(d < 0.0001) 200 else 1)),
            (x, _) => x.inRectangle(Point.zero, canvasSize)
        ).positional
    }

    def randomPointEvo(canvasSize: CanvasSize): Evolution[Point] = {
        inRectangle(canvasSize).flatMap { point =>
            solveIndependent(point)(ring(2).slowDown(5)).positional.take(200)
        }
    }

    def drops(canvasSize: CanvasSize): Evolution[Point] = {
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

    def randomAccWithFriction(canvasSize: CanvasSize): Evolution[Point] = {
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

    def brownianStraight(canvasSize: CanvasSize): Evolution[Point] = {
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

    def duplication(canvasSize: CanvasSize): Evolution[Point] = {
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

    def singlePoint(canvasSize: CanvasSize): Evolution[Point] = {
        constant(canvasSize/2)
    }

    def tinySegments(canvasSize: CanvasSize): Evolution[Point] = {
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

    def circularSegments(canvasSize: CanvasSize): Evolution[Point] = {
        val omega = 0.003
        val k = 0.001 / omega
        val noiseStrength = .05 / omega
        val noise = solveIndependent[Double](100)(ball(noiseStrength)).positional

        centeredIn(canvasSize / 2) {
            solve0Static(Point(300, 0))(_.rotate(omega)).take((2 * Math.PI / omega).toInt + 1).zipWith(noise) {
                case ((position, velocity), d) =>
                    val rotatedVel = velocity.rotate(Math.PI / 2)
                    segment(position - rotatedVel * (k + d), position + rotatedVel * (k + d), 1)
            }.flatten
        }
    }

    def horizontalSegments(canvasSize: CanvasSize): Evolution[Point] = {
        val amplitudeAcc = 0.0002
        val speed = 3
        val segmentSpeed = 1
        val k = 0
        val friction = .06
        val noiseK = .1

        val accelerationEv: AccelerationEvolution[Double] = ball(1) map { d =>
            (position, velocity) => noiseK * d - friction * velocity - k * position
        }

        val amplitudes = solve2(10, 0.0)(accelerationEv).positional.map(d => Point(0, d))

        def vibration(from: Point): Evolution[Point] = uniformLinear(from, Point(speed, 0)).zipWith(amplitudes) { (p, amplitude) =>
            segment(p - amplitude, p + amplitude, segmentSpeed)
        }.flatten

        sequenceParallel(
            Point.sequence(30, Point.zero, canvasSize.copy(x = 0)).map(vibration)
        ).flattenList
    }

    def dependentLines(canvasSize: CanvasSize): Evolution[Point] = {
        val gap = Point(0, 3)
        val x = solveIndependentStatic(0.0)(.5).positional
        val y = solve2(0.0, 0.0)(double.map( _ * .02).map {
            d => (pos, vel) =>
                if (pos < 10 && pos >= 0) d
                else {
                    - Math.signum(pos) * Math.abs(d)
                }
        }).positional
        val point = cartesian(x, y)

        def doubleLine(points: Evolution[List[Point]]): Evolution[List[Point]] =
            points.zipWith(point) { (ps, p3) => Point(ps.head.x, ps.head.y + p3.y) + gap :: ps }

        def multiLine(n: Int): Evolution[List[Point]] =
            n match {
                case _ if n <= 1 => point.map(List(_))
                case _ => doubleLine(multiLine(n - 1))
            }

        centeredIn(Point(0, 400)) {
            multiLine(100).flattenList
        }
    }

    def bouncing(canvasSize: CanvasSize): Evolution[Point] = {
        val ground = canvasSize.y - 100
        val g = .000001
        val elasticity = .000001
        val friction = .0001
        val horizontalSpeed = 0.003

        val gravityField: AccelerationEvolution[Double] =  constant {
            (_, _) => g
        }

        val floor: AccelerationEvolution[Double] = constant {
            (y, vel) =>
                if (y > ground) (ground - y) * elasticity + g - friction * vel
                else 0
        }

        val law = gravityField + floor

        val xEv = solve2IndependentStatic(0.0, horizontalSpeed)(0).positional
        val yEv = solve2(0.0, 0.0) { law }.positional

        cartesian(xEv, yEv)
    }

    def drawingList: DrawingList[Point] = {
        DrawingList.empty[Point]
          .withDrawing(Drawing("brownian speed ring", brownianSpeedRing))
          .withDrawing(Drawing("drops", drops))
    }

    def current(canvasSize: CanvasSize): Evolution[Point] = {
        curlyRing(canvasSize)
//        brownianSpeedRing(canvasSize)
//        drops(canvasSize)
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
//        circularSegments(canvasSize)
//        horizontalSegments(canvasSize)
//        dependentLines(canvasSize)
//        bouncing(canvasSize)
    }
}
