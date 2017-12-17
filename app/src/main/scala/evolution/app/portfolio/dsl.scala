package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.{ConfigComponent, DrawingDSLComponent}
import evolution.app.react.component.config.instances._
import evolution.algebra._
import evolution.geometry.Point
import evolution.algebra.syntax.all._
import evolution.app.codec.JsonCodec
import evolution.app.codec.JsonCodec._
import evolution.app.codec.config.DrawingJsonCodec
import evolution.drawing.algebra.Drawing
import io.circe.generic.auto._
import evolution.app.react.component.config.instances
import evolution.drawing.algebra.interpreter.{Serializer, ToEvolution}
import evolution.drawing.algebra.interpreter.Builder._
import evolution.drawing.algebra.parser.DrawingParser.PointDrawingParser
import io.circe.{Decoder, Encoder}

object dsl extends DrawingDefinition[Point] {
  val name = "drawing dsl"

  type Config = Drawing[Point]

  override val configComponent: ConfigComponent[Config] =
    DrawingDSLComponent.apply(instances.stringConfig)

  def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
    config.run(ToEvolution)
  }

  val initialConfig = integrate(Point.zero, point(rnd(-1, 1), rnd(-1, 1)))

  override def configCodec: JsonCodec[Config] =
    new DrawingJsonCodec[Point](PointDrawingParser, Serializer)
}