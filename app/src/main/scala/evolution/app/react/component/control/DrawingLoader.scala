package evolution.app.react.component.control

import japgolly.scalajs.react
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^.*
import evolution.app.model.Drawing
import evolution.app.react.component.presentational.Select
import evolution.app.portfolio.Portfolio
import evolution.app.react.component.presentational.Select.Item
import evolution.app.data.PointedSeq

object DrawingLoader:
  case class Props(selected: StateSnapshot[Option[Drawing]]):
    val itemsSnapshot: StateSnapshot[PointedSeq[Item[Drawing]]] =
      selected.zoomState(pointedSeq)(seq => _ => Some(seq.selected.value))

  private val portfolioComponent = Select.component[Drawing]
  private val portfolioItems =
    Portfolio.drawings.map(drawing => Item(drawing.title.getOrElse(""), drawing.title.getOrElse("undefined"), drawing))
  private val selectedDrawing = portfolioItems.head
  private val initialSeq = PointedSeq(portfolioItems, selectedDrawing)

  private def pointedSeq(drawing: Option[Drawing]): PointedSeq[Item[Drawing]] =
    drawing.fold(initialSeq)(drawing =>
      initialSeq.selectByPredicate(_.value.drawingState.code == drawing.drawingState.code)
    )

  val component = react.ScalaComponent
    .builder[Props]("load drawing")
    .stateless
    .render_P { p =>
      portfolioComponent(p.itemsSnapshot)
    }
    .build
