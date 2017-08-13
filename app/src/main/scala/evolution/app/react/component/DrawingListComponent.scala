package evolution.app.react.component

import evolution.app.model.legacy.{Drawing, DrawingList}
import evolution.app.portfolio.EvolutionPortfolio
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import paint.geometry.Geometry.Point
import japgolly.scalajs.react._

object DrawingListComponent {
    case class Props(
        currentDrawing: Drawing[Point],
        drawingList: DrawingList[Point],
        onSelect: Drawing[Point] => Callback
    )

    class Backend(bs: BackendScope[Props, Unit]) {
        def render(props: Props): VdomElement = {
            import japgolly.scalajs.react.vdom.html_<^._
            val drawingList = props.drawingList
            val options = drawingList.drawings.values.map { drawing =>
                <.option(drawing.name)
            }
            <.select(
                options.toTagMod,
                ^.onChange ==> onNewSelection(props),
                ^.value := props.currentDrawing.name
            )
        }

        def onNewSelection(props: Props)(e: ReactEventFromInput): Callback =
            props.onSelect(props.drawingList.drawing(e.target.value).get)
    }

    val component = ScalaComponent.builder[Props]("Example")
        .renderBackend[Backend]
        .build
}
