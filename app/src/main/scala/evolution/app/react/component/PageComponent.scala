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
import evolution.geometry.Point
import evolution.app.ReactApp.{LoadDrawingPage, MyPages}
import japgolly.scalajs.react.extra.router.RouterCtl
import io.circe._, io.circe.parser._

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
      string.split("/").toList match {
        case serializedComponent :: seed :: Nil =>
          println(serializedComponent)
          val jsonString = new String(java.util.Base64.getDecoder.decode(serializedComponent))
          for {
            json <- parse(jsonString).toOption
            component <- DrawingComponent.unserialize(Conf.drawingContext, json)
            seedLong <- Try(seed.toLong).toOption
          } yield UrlState(seedLong, component)
        case _ => None
      }
    }

    def serialize(urlState: UrlState): String = {
      val jsonComponent = urlState.drawingComponent.serialize.noSpaces
      val base64Component = java.util.Base64.getEncoder.encodeToString(jsonComponent.getBytes())
      s"$base64Component/${urlState.seed}"
    }
  }

  case class Props(
    router: RouterCtl[MyPages],
    urlState: UrlState
  )

  class Backend(bs: BackendScope[Props, State]) {
    def render(props: Props, state: State): VdomElement = {
      <.div(
        NavbarComponent.component(NavbarComponent.Props(
          <.span(s"${ state.pointRateCounter.rate.toInt } p/s"),
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
          .copy(currentDrawing = definitionToComponent.toComponentWithInitialConfig(definition, state.drawingContext))
      } >> refresh
    }

    private def refresh: Callback = {
      bs.modState { state =>
        state.copy(seed = Random.nextLong())
      }
    }
  }

  def stateFromProps(props: Props): State = {
    State(
      Drawer(
        1000,
        1
      ),
      props.urlState.drawingComponent,
      Conf.drawingList.copy(current = Conf.drawingList.byName(props.urlState.drawingComponent.name)),
      Conf.drawingContext,
      RateCounter.empty(1000),
      props.urlState.seed
    )
  }

  val component = ScalaComponent.builder[Props]("Page")
    .initialStateFromProps(stateFromProps)
    .renderBackend[Backend]
    .shouldComponentUpdate { s =>
      CallbackTo.pure {
        s.currentState.key != s.nextState.key
      }
    }
    .componentDidUpdate { x =>
      x.currentProps.router.set(
        LoadDrawingPage(
          UrlState(
            x.currentState.seed,
            x.currentState.currentDrawing
          )
        )
      )
    }
    .componentWillReceiveProps { x =>
      if (UrlState.serialize(x.nextProps.urlState) != UrlState.serialize(x.currentProps.urlState)) {
        x.setState(stateFromProps(x.nextProps)) >> Callback.log("Props changed")
      } else Callback.empty
    }
    .build
}
