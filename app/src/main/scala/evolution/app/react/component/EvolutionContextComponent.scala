package evolution.app.react.component

import evolution.app.model.legacy.{Drawing, DrawingList}
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.VdomElement
import evolution.app.portfolio.EvolutionGeneratorPortfolio.brownian
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.html_<^._
import paint.geometry.Geometry.Point

trait EvolutionContextComponent[Context] {
  def component(props: EvolutionContextComponentProps[Context]): VdomElement
}

case class EvolutionContextComponentProps[Context](
  context: Context,
  callback: Context => Callback
)

object instances {
  object BrownianComponent extends EvolutionContextComponent[brownian.Context] {
    type Context = brownian.Context
    type Props = EvolutionContextComponentProps[brownian.Context]

    class Backend(bs: BackendScope[EvolutionContextComponentProps[brownian.Context], Unit]) {
      def render(props: Props): VdomElement = {
        <.div(
          DoubleInputComponent.component(DoubleInputComponent.Props(props.context.radius, onRadiusChange(props))),
          DoubleInputComponent.component(DoubleInputComponent.Props(props.context.start.x, onXChange(props))),
          DoubleInputComponent.component(DoubleInputComponent.Props(props.context.start.y, onYChange(props)))
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
