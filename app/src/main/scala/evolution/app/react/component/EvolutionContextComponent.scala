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
  def component(context: Context, callback: Context => Callback): VdomElement
}

case class EvolutionContextComponentProps[Context](
  context: Context,
  callback: Context => Callback
)

object instances {

  class Backend(bs: BackendScope[EvolutionContextComponentProps[brownian.Context], Unit]) {
    def render(props: EvolutionContextComponentProps[brownian.Context]): VdomElement = {
      <.div(
        DoubleInputComponent.component(DoubleInputComponent.Props(props.context.radius, onRadiusChange)),
        DoubleInputComponent.component(DoubleInputComponent.Props(props.context.start.x, onXChange)),
        DoubleInputComponent.component(DoubleInputComponent.Props(props.context.start.y, onYChange))
      )
    }

    private def onRadiusChange(radius: Double): Callback = {
      ???
    }

    private def onXChange(x: Double): Callback = {
      ???
    }

    private def onYChange(y: Double): Callback = {
      ???
    }
  }

  val brownianContextComponent = new EvolutionContextComponent[brownian.Context] {
    override def component(context: brownian.Context, callback: (brownian.Context) => Callback): VdomElement =
      <.div(
        NumericInputComponent.component()
      )
  }


}
