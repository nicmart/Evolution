package evolution.app.model.configured

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.VdomElement
import evolution.algebra.materializer.Materializer
import evolution.app.conf.Conf
import evolution.app.model.context.DrawingContext
import evolution.app.codec
import evolution.app.model.definition.DrawingDefinition
import evolution.geometry.Point
import io.circe.Json
import io.circe.syntax._

trait DrawingComponent[S, T] {
  def name: String
  def materialize(seed: S): Stream[T]
  def configElement(onChange: DrawingComponent[S, T] => Callback): VdomElement
  def serialize: Json
}

object DrawingComponent {

  class JsonCodec(
    drawingContext: DrawingContext,
    drawing: DrawingDefinition[Point],
    definitionToComponent: DefinitionToComponent[Long, Point]
  ) extends codec.JsonCodec[DrawingComponent[Long, Point]] {

    override def encode(component: DrawingComponent[Long, Point]): Json =
      component.serialize

    override def decode(json: Json): Option[DrawingComponent[Long, Point]] = {
      for {
        config <- drawing.configCodec.decode(json)
      } yield definitionToComponent.toComponent(drawing, drawingContext)(config)
    }
  }
}

class EvolutionDrawingComponent[S, T, Config](
  materializer: Materializer[S],
  configuredDrawing: ConfiguredDrawing[T, Config]
) extends DrawingComponent[S,T] {

  override def name: String = configuredDrawing.drawing.name

  override def configElement(callback: DrawingComponent[S, T] => Callback): VdomElement = {
    configuredDrawing.configElement(config => callback(withConfig(config)))
  }

  override def materialize(seed: S): Stream[T] =
    materializer.materialize(seed, configuredDrawing.evolution)

  override def serialize: Json =
    configuredDrawing.drawing.configCodec.encode(configuredDrawing.config)

  private def withConfig(config: Config): DrawingComponent[S, T] =
    new EvolutionDrawingComponent[S, T, Config](
      materializer,
      configuredDrawing.copy(config = config)
    )
}



