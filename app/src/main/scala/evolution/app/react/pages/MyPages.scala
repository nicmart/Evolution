package evolution.app.react.pages

import evolution.app.model.state.DrawingState
import evolution.app.conf.Conf

sealed trait MyPages

case object Home extends MyPages
case class LoadDrawingPage(state: DrawingState[Conf.drawingDefinition.Config]) extends MyPages
case object NotFound extends MyPages