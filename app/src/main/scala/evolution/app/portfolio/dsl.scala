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
import evolution.drawing.algebra._
import io.circe.generic.auto._
import evolution.app.react.component.config.instances
import evolution.drawing.algebra.interpreter.{Serializer, ToEvolution}
import evolution.drawing.algebra.interpreter.Builder._
import evolution.drawing.algebra.parser.DrawingParser.PointDrawingParser
import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._

object dsl extends DrawingDefinition[Point] {
  val name = "drawing dsl"

  case class Config(
    dsl: Drawing[Unit, Point]
  )

  override val configComponent: ConfigComponent[Config] = {
    implicit val dslComponent: ConfigComponent[Drawing[Unit, Point]] =
      DrawingDSLComponent.apply(instances.textConfig)
    ConfigComponent[Config]
  }

  def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
    config.dsl.run(ToEvolution)(())
  }

  val initialConfig = Config(
    integrate[Empty, Point](Point.zero, point(rnd(-1, 1), rnd(-1, 1)))
  )

  override def configCodec: JsonCodec[Config] = {
    implicit val drawingCodec: DrawingJsonCodec[Point] =
      new DrawingJsonCodec[Point](PointDrawingParser, Serializer)
    import circeImplicits._
    JsonCodec[Config]
  }

}