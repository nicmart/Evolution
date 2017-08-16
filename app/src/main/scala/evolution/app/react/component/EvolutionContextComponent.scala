package evolution.app.react.component

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.VdomElement
import evolution.app.portfolio.EvolutionGeneratorPortfolio.brownian
import evolution.app.react.component.presentational.{DoubleInputComponent, NumericInputComponent}
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.html_<^._

trait EvolutionContextComponent[Context] {
  def component(props: EvolutionContextComponent.Props[Context]): VdomElement
}

object EvolutionContextComponent {
  case class Props[Context](
    context: Context,
    callback: Context => Callback
  )
}




object instances {
  implicit val doubleContext: EvolutionContextComponent[Double] = new EvolutionContextComponent[Double] {
    override def component(props: EvolutionContextComponent.Props[Double]): VdomElement =
      DoubleInputComponent(props.context, props.callback)
  }

  implicit val intContext: EvolutionContextComponent[Int] = new EvolutionContextComponent[Int] {
    override def component(props: EvolutionContextComponent.Props[Int]): VdomElement =
      NumericInputComponent(props.context, props. callback)
  }

  object BrownianComponent extends EvolutionContextComponent[brownian.Context] {
    type Context = brownian.Context
    type Props = EvolutionContextComponent.Props[Context]

    class Backend(bs: BackendScope[Props, Unit]) {
      def render(props: Props): VdomElement = {
        <.div(
          DoubleInputComponent(props.context.radius, onRadiusChange(props)),
          DoubleInputComponent(props.context.start.x, onXChange(props)),
          DoubleInputComponent(props.context.start.y, onYChange(props))
        )
      }

      private def onRadiusChange(props: Props)(radius: Double): Callback = {
        props.callback(props.context.copy(radius = radius))
      }

      private def onXChange(props: Props)(x: Double): Callback = {
        val currentStart = props.context.start
        props.callback(props.context.copy(start = currentStart.copy(x = x)))
      }

      private def onYChange(props: Props)(y: Double): Callback = {
        val currentStart = props.context.start
        props.callback(props.context.copy(start = currentStart.copy(y = y)))
      }
    }

    def component(props: Props): VdomElement =
      ScalaComponent.builder[Props]("Component")
        .renderBackend[Backend]
        .build
        .apply(props)
  }
}
