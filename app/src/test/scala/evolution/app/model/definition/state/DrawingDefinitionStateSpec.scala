package evolution.app.model.definition.state

import evolution.algebra
import evolution.algebra.Evolution
import evolution.app.codec.JsonCodec
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Point
import org.scalatest._
import evolution.app.react.component.config.componentInstances._

class DrawingDefinitionStateSpec
  extends WordSpec
    with Matchers {

    "this is a test" in {
      1 shouldBe 1
    }

  "a definition should be serializable" should {
    import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

//    val definition = new DrawingDefinition[Point] {
//      case class Config(x: Int, y: Int)
//      override def name = "test"
//      override def initialConfig =
//        Config(12, 13)
//      override def evolution(config: Config, context: DrawingContext): Evolution[Point] = new Evolution[Point] {
//        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] = {
//          import alg._
//          constant(Point.zero)
//        }
//      }
//      override def configComponent: ConfigComponent[Config] = ConfigComponent[Config]
//      override def configCodec(): JsonCodec[Config] = JsonCodec[Config]
//    }
//
//    case class Config(x: Int, y: Int)
//
//    val expectedJson = parse {
//      """
//        |{
//        |  "x": 12,
//        |  "y": 13
//        |}
//      """.stripMargin
//    }.getOrElse(Json.fromString("asdasd"))
  }

}
