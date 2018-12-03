package evolution.app.portfolio
import evolution.app.codec.{ Codec, JsonCodec }
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.model.state.DrawingState
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Point
import evolution.app.react.component.config.instances.unitConfig
import io.circe.Json

import scala.util.Random

object rawBrownian extends DrawingDefinition[Point] {
  val name = "raw brownian"
  override type Config = Unit
  override def initialConfig: Unit = ()

  override def stream(ctx: DrawingContext, state: DrawingState[Unit]): Stream[Point] = {
    streamFrom(Point.zero)
  }

  private def streamFrom(current: Point): Stream[Point] = {
    val next = Point(current.x + Random.nextDouble() * 4 - 2, current.y + Random.nextDouble() * 4 - 2)
    current #:: streamFrom(next)
  }

  override def configComponent: ConfigComponent[Unit] =
    ConfigComponent[Unit]
  override def configCodec: JsonCodec[Unit] =
    Codec.instance[Unit, Json](_ => Json.True, _ => Some(()))
}
