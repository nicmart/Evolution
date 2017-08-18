package evolution.app.portfolio

import evolution.app.canvas.CanvasSize
import evolution.app.model.{Drawing, DrawingList, DrawingListWithSelection}
import evolution.app.portfolio.DrawingPortfolio.drops
import evolution.app.react.component.config.instances._
import evolution.app.react.component.config.{ConfigComponent, ConfiguredComponent}
import paint.evolution.Evolution
import paint.evolution.Evolution.{pure, sequenceParallel}
import paint.evolution.PointEvolutions.{rectangle2D, ring}
import paint.evolution.generator.ConfiguredEvolution
import paint.evolution.motion.MotionEvolutions.solveIndependent
import paint.geometry.Geometry.Point
import paint.evolution.implicits._
import paint.evolution.NumericEvolutions.double
import paint.evolution.motion.{AccelerationLaw, MotionEvolutions}

object DrawingPortfolio {

  trait WithCanvasSize {
    def canvasSize: Point
  }

  abstract class DrawingDefinition(name: String) {
    type Config <: WithCanvasSize
    def evolution(config: Config): Evolution[Point]
    def defaultConfig: Config
    def component: ConfigComponent[Config]
    def drawing: Drawing[Point] =
      Drawing(
        name,
        ConfiguredEvolution(evolution, defaultConfig),
        ConfiguredComponent(component, defaultConfig)
      )
  }

  object brownian extends DrawingDefinition("brownian") {

    case class Config(
      canvasSize: Point,
      radius: Double
    ) extends WithCanvasSize

    override def component: ConfigComponent[Config] =
      ConfigComponent[Config]

    override def evolution(config: Config): Evolution[Point] = {
      solveIndependent(config.canvasSize)(
        rectangle2D(config.radius)
      ).positional
    }

    val defaultConfig = Config(Point(900, 600), 2)
  }

  object brownianWithRandomJumps extends DrawingDefinition("brownian with random jumps") {

    case class Config(
      canvasSize: Point,
      radius: Double,
      jumpProbability: Double,
      jumpSize: Int
    ) extends WithCanvasSize

    override def component: ConfigComponent[Config] =
      ConfigComponent[Config]

    override def evolution(config: Config): Evolution[Point] = {
      val slowDownEvo = double.map[Int] { d =>
        if (d < config.jumpProbability) config.jumpSize else 1
      }
      MotionEvolutions.solveIndependent(config.canvasSize)(
        rectangle2D(config.radius).slowDown(slowDownEvo)
      ).positional
    }

    val defaultConfig =
      Config(
        Point(900, 600),
        1,
        0.0001,
        200
      )
  }

  object drops extends DrawingDefinition("drops") {
    case class Config(
      canvasSize: Point,
      friction: Double,
      acceleration: Double,
      threshold: Double,
      randomForceProbability: Double,
      randomForceStrength: Double,
      numberOfDrops: Int
    ) extends WithCanvasSize

    override def defaultConfig: Config =
      Config(
        canvasSize = Point(1300, 600),
        friction = 0.02,
        acceleration = 0.003,
        threshold = 0.1,
        randomForceProbability = 0.01,
        randomForceStrength = 0.1,
        numberOfDrops = 80
      )

    override def evolution(config: Config): Evolution[Point] = {
        val acc = Point(0, config.acceleration)
        val randomForces = double.flatMap { p =>
          if (p < config.randomForceProbability) ring(config.randomForceStrength).first else pure(Point.zero)
        }

        def accelerationEvolution: Evolution[AccelerationLaw[Point]] = randomForces map { randomAcc =>
          (position, velocity) =>
            if ((randomAcc + velocity + acc).norm() < config.threshold) -velocity
            else randomAcc + acc - velocity * config.friction
        }

        def pointEvo(from: Point): Evolution[Point] = {
          MotionEvolutions.solve2(from, Point.zero)(accelerationEvolution).positional
        }

        val evo = sequenceParallel(
          Point.sequence(config.numberOfDrops, config.canvasSize.copy(y = 0), Point(0, 0)).map(pointEvo)
        ).flattenList

        evo
    }

    override def component: ConfigComponent[Config] = ConfigComponent[Config]
  }

  def listWithSelection: DrawingListWithSelection[Point] =
    DrawingListWithSelection(
      DrawingList(List(
        brownian.drawing,
        brownianWithRandomJumps.drawing,
        drops.drawing
      )),
      brownian.drawing
    )
}
