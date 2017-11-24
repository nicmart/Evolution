package evolution.app.portfolio

import evolution.algebra.Evolution
import evolution.app.codec.JsonCodec
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition.Aux
import evolution.app.model.definition.{DrawingDefinition, DrawingListWithSelection}
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Point
import japgolly.scalajs.react.vdom.VdomElement

class DrawingListDefinition(drawingList: DrawingListWithSelection[Point]) extends DrawingDefinition[Point] {

  trait Config {
    type InnerConfig
    def innerDrawing: DrawingDefinition.Aux[Point, InnerConfig]
    def innerConfig: InnerConfig
  }

  private def configFor(lst: DrawingListWithSelection[Point])(config: lst.current.Config): Config =
    new Config {
      override type InnerConfig = lst.current.Config
      override def innerDrawing: Aux[Point, InnerConfig] = lst.current
      override def innerConfig: InnerConfig = innerConfig
    }

  override def name: String = "All drawings"
  override def initialConfig: Config = configFor(drawingList)(drawingList.current.initialConfig)

  override def evolution(config: Config, context: DrawingContext): Evolution[Point] =
    config.innerDrawing.evolution(config.innerConfig, context)

  override def configComponent: ConfigComponent[Config] = new ConfigComponent[Config] {
    override def element(props: ConfigComponent.Props[Config]): List[VdomElement] = {
      props.config.innerDrawing.configComponent.element(props.config.innerConfig)
    }
  }

  override def configCodec: JsonCodec[Config] = ???
}