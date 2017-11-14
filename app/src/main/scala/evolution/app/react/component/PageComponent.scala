package evolution.app.react.component

import evolution.app.canvas.Drawer
import evolution.app.conf.Conf
import evolution.app.model.configured.{DefinitionToComponent, DrawingComponent}
import evolution.app.model.context.DrawingContext
import evolution.app.model.counter.RateCounter
import evolution.app.model.definition.{DrawingDefinition, DrawingListWithSelection}
import evolution.app.react.component.presentational._
import evolution.app.react.component.presentational.styled.HorizontalFormFieldComponent
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CallbackTo, ScalaComponent}
import org.scalajs.dom
import org.scalajs.dom.Window
import evolution.geometry.Point
import evolution.app.ReactApp.{MyPages, LoadDrawingPage}
import japgolly.scalajs.react.extra.router.RouterCtl

import scala.util.{Random, Try}

object PageComponent {

  def definitionToComponent: DefinitionToComponent[Long, Point] =
    Conf.definitionToComponent

  def canvasInitializer: dom.html.Canvas => Unit =
    Conf.canvasInitializer

  case class State(
    drawer: Drawer,
    currentDrawing: DrawingComponent[Long, Point],
    drawingListWithSelection: DrawingListWithSelection[Point],
    drawingContext: DrawingContext,
    pointRateCounter: RateCounter,
    seed: Long
  ) {
    def points: Stream[Point] =
      currentDrawing.materialize(seed)

    /**
      * Use to determine if the canvas has to be re-rendered
      */
    def canvasKey: String = seed.toString

    /**
      * Used to determine if the page needs an update
      *
      * @return
      */
    def key: Int = (
      seed,
      currentDrawing,
      pointRateCounter.rate,
      drawingListWithSelection.current.name
    ).hashCode()
  }

  case class UrlState(
    seed: Long,
    drawingComponent: DrawingComponent[Long, Point]
  )

  object UrlState {
    def unserialize(string: String): Option[UrlState] = {
      Try {
        string.split("/").toList match {
          case name :: seed :: Nil =>
            UrlState(
              seed.toLong,
              definitionToComponent.toComponent(
                Conf.drawingList.byName(name),
                drawingContext(dom.window)
              )
            )
        }
      }.toOption
    }
    def serialize(urlState: UrlState): String = {
      s"${urlState.drawingComponent.name}/${urlState.seed.toString}"
    }
  }

  case class Props(
    router: RouterCtl[MyPages],
    urlState: Option[UrlState]
  )

  class Backend(bs: BackendScope[Props, State]) {
    def render(props: Props, state: State): VdomElement = {
      <.div(
        NavbarComponent.component(NavbarComponent.Props(
          <.span(s"${state.pointRateCounter.rate.toInt} p/s"),
          ButtonComponent.component(ButtonComponent.Props(
            "Refresh",
            refresh
          )),
          HorizontalFormFieldComponent.component(HorizontalFormFieldComponent.Props(
            "Iterations",
            "",
            IntInputComponent(state.drawer.iterations, onIterationsChanged)
          )
          ),
          HorizontalFormFieldComponent.component(HorizontalFormFieldComponent.Props(
            "Drawing",
            "",
            DrawingListComponent.component(
              DrawingListComponent.Props(
                state.drawingListWithSelection.list,
                state.drawingListWithSelection.current,
                onDrawingDefinitionChange
              )
            )
          )
          )
        )
        ),
        <.div(
          ^.id := "page-content",
          CanvasComponent.component.withKey(state.canvasKey)(CanvasComponent.Props(
            state.drawingContext,
            canvasInitializer,
            state.drawer,
            state.points,
            bs.modState { state =>
              state.copy(pointRateCounter = state.pointRateCounter.count(state.drawer.iterations))
            }
          )
          ),
          SidebarComponent.component.withKey(state.currentDrawing.name)(
            SidebarComponent.Props(
              active = true,
              state.currentDrawing.configElement(onConfiguredDrawingChange)
            )
          )
        )
      )
    }

    private def onIterationsChanged(value: Int): Callback = {
      bs.modState { state =>
        state
          .copy(drawer = state.drawer.copy(iterations = value))
      } >> refresh
    }

    private def onConfiguredDrawingChange(drawingComponent: DrawingComponent[Long, Point]): Callback = {
      bs.modState { state =>
        state
          .copy(currentDrawing = drawingComponent)
      } >> refresh
    }

    private def onDrawingDefinitionChange(definition: DrawingDefinition[Point]): Callback = {
      bs.modState { state =>
        state
          .copy(drawingListWithSelection = state.drawingListWithSelection.copy(current = definition))
          .copy(currentDrawing = definitionToComponent.toComponent(definition, state.drawingContext))
      } >> refresh
    }

    private def refresh: Callback = {
      updateSeed >> updateUrl
    }

    private def updateSeed: Callback = {
      bs.modState { state =>
        state.copy(seed = Random.nextLong())
      }
    }

    private def updateUrl: Callback =
      for {
        state <- bs.state
        props <- bs.props
        router = props.router
        _ <- router.set(LoadDrawingPage(UrlState(state.seed, state.currentDrawing)))
      } yield ()
  }

  val initialUrlState =
    UrlState(
      Random.nextLong(),
      definitionToComponent.toComponent(
        Conf.drawingList.current,
        drawingContext(dom.window)
      )
    )

  def initialState(props: Props): State = {
    val urlState = props.urlState.getOrElse(initialUrlState)
    State(
      Drawer(
        1000,
        1
      ),
      urlState.drawingComponent,
      Conf.drawingList.copy(current = Conf.drawingList.byName(urlState.drawingComponent.name)),
      drawingContext(dom.window),
      RateCounter.empty(1000),
      urlState.seed
    )
  }

  val component = ScalaComponent.builder[Props]("Page")
    .initialStateFromProps(initialState)
    .renderBackend[Backend]
    .shouldComponentUpdate(s => CallbackTo.pure(s.currentState.key != s.nextState.key))
    .build

  private def drawingContext(window: Window): DrawingContext = {
    val document = window.document
    DrawingContext(
      DrawingContext.CanvasSize(
        2 * Math.max(document.documentElement.clientWidth, window.innerWidth).toInt,
        2 * Math.max(document.documentElement.clientHeight, window.innerHeight).toInt
      )
    )
  }
}
