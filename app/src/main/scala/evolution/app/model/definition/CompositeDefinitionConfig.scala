package evolution.app.model.definition

import evolution.app.codec.JsonCodec
import evolution.app.data.PointedSeq
import io.circe.Json

sealed trait CompositeDefinitionConfig[T] {
  type InnerConfig
  def config: InnerConfig
  def definition: DrawingDefinition.Aux[T, InnerConfig]
  override def hashCode(): Int =
    (config, definition).hashCode()
}

object CompositeDefinitionConfig {
  def apply[T, C](cfg: C, dfn: DrawingDefinition.Aux[T, C]): CompositeDefinitionConfig[T] =
    new CompositeDefinitionConfig[T] {
      override type InnerConfig = C
      override def definition: DrawingDefinition.Aux[T, C] = dfn
      override def config: C = cfg
    }

  def jsonCodec[T](drawingList: PointedSeq[DrawingDefinition[T]]): JsonCodec[CompositeDefinitionConfig[T]] =
    new JsonCodec[CompositeDefinitionConfig[T]] {
      override def encode(config: CompositeDefinitionConfig[T]): Json = Json.obj(
        "name" -> Json.fromString(config.definition.name),
        "config" -> config.definition.configCodec.encode(config.config)
      )
      override def decode(json: Json): Option[CompositeDefinitionConfig[T]] = {
        val cursor = json.hcursor
        for {
          name <- cursor.downField("name").as[String].toOption
          drawing = drawingList.get(_.name == name)
          configJson <- cursor.downField("config").focus
          config <- drawing.configCodec.decode(configJson)
        } yield CompositeDefinitionConfig[T, drawing.Config](config, drawing)
      }
    }
}
