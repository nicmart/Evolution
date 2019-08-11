package evolution.app.react.component

import evolution.geometry.Point
import evolution.app.model.context.DrawingContext
import evolution.app.react.component.presentational.Editor
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import evolution.app.model.CodeCompiler

object Code {

  case class Props(
    code: StateSnapshot[String],
    ctx: DrawingContext,
    seed: Long,
    newIterator: Iterator[Point] => Callback
  ) {
    def key: Int = (code.value, ctx, seed).hashCode()
  }

  case class State(error: Option[String])

  class Backend {
    def render(props: Props, state: State): VdomElement = {
      Editor.component(Editor.Props(props.code, state.error))
    }
  }

  private def processProps(props: Props, previousProps: Option[Props], setState: State => Callback): Callback =
    if (previousProps.exists(_.key != props.key)) {
      CodeCompiler
        .compile(
          props.code.value,
          props.seed,
          props.ctx
        )
        .fold(
          error => setState(State(Some(error))),
          iterator => props.newIterator(iterator) >> setState(State(None))
        )
    } else Callback.empty

  val component =
    ScalaComponent
      .builder[Props]("Code")
      .initialState(State(None))
      .backend[Backend](_ => new Backend)
      .render(scope => scope.backend.render(scope.props, scope.state))
      .componentDidMount(s => processProps(s.props, None, s.setState))
      .componentWillReceiveProps(s => processProps(s.nextProps, Some(s.currentProps), s.setState))
      .build
}
