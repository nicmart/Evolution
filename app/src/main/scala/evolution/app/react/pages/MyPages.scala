package evolution.app.react.pages

import evolution.app.model.state.DrawingState

sealed trait MyPages[+C]

case object Home extends MyPages[Nothing]
case class LoadDrawingPage[C](state: DrawingState[C]) extends MyPages[C]
case object NotFound extends MyPages[Nothing]