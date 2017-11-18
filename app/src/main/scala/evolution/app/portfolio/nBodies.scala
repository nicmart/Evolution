package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.portfolio.primes.Config
import evolution.app.react.component.config.{ConfigCodec, ConfigComponent}
import evolution.algebra
import evolution.geometry.Point
import evolution.app.react.component.config.componentInstances._
import evolution.algebra.Evolution
import evolution.algebra.MotionEvolutionAlgebra.{AccelerationLaw, PhaseSpace, Position, Velocity}
import evolution.algebra.syntax.all._
import evolution.app.portfolio.bouncing.Config
import io.circe.generic.auto._

object nBodies extends DrawingDefinition[Point] {
  val name = "n bodies"

  case class Config(
    gravityConstant: Double,
    distance: Double,
    mass1: Double,
    speed1: Double,
    mass2: Double,
    speed2: Double
  )

  def initialConfig = Config(
    gravityConstant = 0.01,
    distance = 200,
    speed1 = 0,
    mass1 = 10000,
    speed2 = 0.5,
    mass2 = 1
  )

  class ThisEvolution(config: Config, context: DrawingContext) extends Evolution[Point] {
    override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] = {
      import alg._
      type FieldGen = PhaseSpace[Point] => AccelerationLaw[Point]
      type Mass = Double

      def solve2Multi(
        bodyStates: IndexedSeq[(PhaseSpace[Point], Mass)],
        fields: IndexedSeq[FieldGen]
      ): Evo[IndexedSeq[PhaseSpace[Point]]] = {
        val currentLaws: IndexedSeq[(AccelerationLaw[Point], Mass)] = laws(bodyStates, fields)
        val accs = currentLaws.zip(bodyStates).map { case ((law, mass1), ((pos, vel), mass2)) => law(pos, vel) }
        val nextStates: IndexedSeq[(PhaseSpace[Point], Mass)] = bodyStates.zip(accs).map {
          case (((pos, vel), mass), acc) =>
            ((pos + vel, vel + acc), mass)
        }
        pure(bodyStates.map(_._1)).append(solve2Multi(nextStates, fields))
      }

      def laws(
        from: IndexedSeq[(PhaseSpace[Point], Mass)],
        fields: IndexedSeq[FieldGen]
      ): IndexedSeq[(AccelerationLaw[Point], Mass)] = {
        aggregateLaws(from.zip(fields).map { case ((phase, mass), fieldGen) => (fieldGen(phase), mass) })
      }

      def aggregateLaws(
        laws: IndexedSeq[(AccelerationLaw[Point], Mass)]
      ): IndexedSeq[(AccelerationLaw[Point], Mass)] = {
        for (i <- laws.indices) yield {
          val otherLaws = for (j <- laws.indices if i != j) yield laws(j)

          // @TODO extract in motion package
          val sumLaw: AccelerationLaw[Point] = { (pos, vel) =>
            otherLaws.foldLeft(Point.zero)((acc1, lawWithMass) => acc1 + lawWithMass._1(pos, vel) / laws(i)._2)
          }

          (sumLaw, laws(i)._2)
        }
      }

      import config._

      val start = IndexedSeq(
        ((Point(-distance / 2, 0), Point(0, speed1)), mass1),
        ((Point(distance / 2, 0), Point(0, speed2)), mass2)
      )
      val fieldGens: IndexedSeq[FieldGen] = IndexedSeq(fieldGen(gravityConstant, mass1), fieldGen(gravityConstant, mass2))
      val evo: Evo[IndexedSeq[(Position[Point], Velocity[Point])]] = solve2Multi(start, fieldGens)

      centeredIn(context.canvasSize.point / 2) {
        evo.map(_.toList).flattenList.positional
      }
    }
  }

  def evolution(config: Config, context: DrawingContext): Evolution[Point] =
    new ThisEvolution(config, context)

  def configComponent = ConfigComponent[Config]

  override def configCodec: ConfigCodec[Config] =
    ConfigCodec[Config]

  private def fieldGen(gravityConstant: Double, mass: Double)(phase: PhaseSpace[Point]): AccelerationLaw[Point] = {
    val (pos1, vel1) = phase
    (pos2, vel2) => {
      val r12 = pos2 - pos1
      -r12 * gravityConstant * mass / Math.pow(r12.norm(), 3)
    }
  }
}
