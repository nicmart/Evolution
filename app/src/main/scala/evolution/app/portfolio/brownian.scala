package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.config.instances._
import paint.evolution.Evolution
import paint.evolution.algebra.{EvolutionAlgebra, FullAlgebra, MotionEvolutionAlgebra, PointEvolutionAlgebra}
import paint.evolution.motion.MotionEvolutions
import paint.geometry.Geometry.Point

object brownian extends DrawingDefinition("brownian") {

  case class Config(
    radius: Double
  )

  override def component: ConfigComponent[Config] =
    ConfigComponent[Config]

  override def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
    import paint.evolution.PointEvolutions.rectangle2D
    import paint.evolution.implicits._
    MotionEvolutions.solveIndependent(context.canvasSize.point / 2)(
      rectangle2D(config.radius)
    ).positional
  }

  trait EvolutionDescription[A] {
    def run[Evo[+_]](implicit alg: FullAlgebra[Evo]): Evo[A]
  }


  def evo(config: Config, context: DrawingContext): EvolutionDescription[Point] = new EvolutionDescription[Point] {

    override def run[Evo[+_]](
      implicit alg: FullAlgebra[Evo]
    ): Evo[Point] = {
      import alg._
      import paint.evolution.algebra.syntax.all._
      solveIndependent(context.canvasSize.point / 2)(
        rectangle2D(config.radius)
      ).positional
    }
  }

  val currentConfig = Config(2)
}
