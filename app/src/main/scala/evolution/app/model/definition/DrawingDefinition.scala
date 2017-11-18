package evolution.app.model.definition

import evolution.app.model.context.DrawingContext
import evolution.app.react.component.config.{ConfigCodec, ConfigComponent}
import evolution.algebra.Evolution
import evolution.geometry.Point
import io.circe.Json

trait DrawingDefinition[T] {
  type Config
  def name: String
  def initialConfig: Config
  def evolution(config: Config, context: DrawingContext): Evolution[T]
  def configComponent: ConfigComponent[Config]
  def configCodec: ConfigCodec[Config] =
    ConfigCodec.instance(
      config => Json.fromString(name),
      json => Some(initialConfig)
    )
}

object DrawingDefinition {
  type Aux[T, C] = DrawingDefinition[T] { type Config = C }
}

final case class DrawingListWithSelection[T](
  list: List[DrawingDefinition[T]],
  current: DrawingDefinition[T]
) {
  def byName(name: String): DrawingDefinition[T] =
    list.find(_.name == name).getOrElse(current)
}