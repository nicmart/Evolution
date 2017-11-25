package evolution.app.portfolio

import evolution.algebra.Evolution
import evolution.app.codec.JsonCodec
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.{DrawingDefinition, DrawingListWithSelection}
import evolution.app.react.component.DrawingListComponent
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.presentational.styled.HorizontalFormFieldComponent
import evolution.geometry.Point
import io.circe.{Decoder, Encoder, HCursor, Json}
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

class DrawingListDefinition(drawingList: DrawingListWithSelection[Point]) extends DrawingDefinition[Point] {

  sealed abstract class Config {
    type InnerConfig
    def configC: ConfigC[InnerConfig]
    def withConfig[C](config: ConfigC[C]): Config = new Config {
      type InnerConfig = C
      val configC: ConfigC[InnerConfig] = config
    }
  }

  object Config {
    def apply[C](
      innerDrawing: DrawingDefinition.Aux[Point, C],
      allDrawings: List[DrawingDefinition[Point]],
      innerConfig: C
    ): Config =
      new Config {
        override type InnerConfig = C
        override def configC: ConfigC[C] =
          ConfigC(innerDrawing, allDrawings, innerConfig)
      }
  }

  case class ConfigC[C](
    innerDrawing: DrawingDefinition.Aux[Point, C],
    allDrawings: List[DrawingDefinition[Point]],
    innerConfig: C
  )

  override def name: String = "All drawings"
  override def initialConfig: Config =
    Config[drawingList.current.Config](
      drawingList.current,
      drawingList.list,
      drawingList.current.initialConfig
    )

  override def evolution(config: Config, context: DrawingContext): Evolution[Point] =
    config.configC.innerDrawing.evolution(config.configC.innerConfig, context)

  override def configComponent: ConfigComponent[Config] = new ConfigComponent[Config] {
    override def element(props: ConfigComponent.Props[Config]): List[VdomElement] = {
      val config = props.config
      val innerComponent: ConfigComponent[config.InnerConfig] =
        config.configC.innerDrawing.configComponent
      val innerConfig = config.configC.innerConfig

      val innerDom = innerComponent.element(ConfigComponent.Props[config.InnerConfig](
        innerConfig,
        newInnerConfig => props.callback(config.withConfig(config.configC.copy(innerConfig = newInnerConfig)))
      ))

      val dropdown = HorizontalFormFieldComponent.component(HorizontalFormFieldComponent.Props(
        "Drawing",
        "",
        DrawingListComponent.component(
          DrawingListComponent.Props(
            props.config.configC.allDrawings,
            props.config.configC.innerDrawing,
            newDefinition => props.callback(
              Config[newDefinition.Config](
                newDefinition,
                drawingList.list,
                newDefinition.initialConfig
              )
            )
          )
        )
      ))

      dropdown :: innerDom
    }
  }

  override def configCodec: JsonCodec[Config] = {
    new JsonCodec[Config] {
      override def encode(config: Config): Json = Json.obj(
        "name" -> Json.fromString(config.configC.innerDrawing.name),
        "config" -> config.configC.innerDrawing.configCodec.encode(config.configC.innerConfig)
      )
      override def decode(json: Json): Option[Config] = {
        val cursor = json.hcursor
        for {
          name <- cursor.downField("name").as[String].toOption
          drawing = drawingList.byName(name)
          configJson <- cursor.downField("config").focus
          config <- drawing.configCodec.decode(configJson)
        } yield Config[drawing.Config](drawing, drawingList.list, config)
      }
    }
  }
}