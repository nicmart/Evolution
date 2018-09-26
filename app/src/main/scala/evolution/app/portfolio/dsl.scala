package evolution.app.portfolio

import evolution.algebra._
import evolution.app.codec.JsonCodec
import evolution.app.codec.config.DrawingJsonCodec
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent.instance
import evolution.app.react.component.config.{ConfigComponent, instances}
import evolution.geometry.Point
import evolution.primitive.algebra
import evolution.primitive.algebra.DrawingAlgebra
import evolution.primitive.algebra.interpreter._
import evolution.primitive.algebra.parser.DrawingAlgebraParser
import fastparse.noApi
import japgolly.scalajs.react.vdom.html_<^._

object dsl extends DrawingDefinition[Point] {
  val name = "drawing dsl"

  trait Config {
    def run[S[_], F[_], R[_]](alg: DrawingAlgebra[S, F, R, String]): R[F[Point]]
  }

  override val configComponent: ConfigComponent[Config] = {
    // TODO improve this rubbish
    instance[Config]("drawing config") { (config2Snapshot, children) =>
      val stringSnapshot =
        config2Snapshot.zoomState[String](config2 => config2.run(algebra.interpreter.Serializer)(Nil)) {
          serialized => previousConfig =>
            new Config {
              override def run[S[_], F[_], R[_]](alg: DrawingAlgebra[S, F, R, String]): R[F[Point]] = {
                val algebraParser = new DrawingAlgebraParser[S, F, R](alg)
                val parser: noApi.Parser[R[F[Point]]] = algebraParser.container.pointParserF
                parser
                  .parse(serialized)
                  .fold((_, _, failure) => { println(failure); previousConfig.run(alg) }, (drawing, _) => drawing)
              }
            }
        }
      val component: ConfigComponent[String] = instances.textConfig
      component(stringSnapshot)()
    }
  }

  def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
    //config.run(new ToEvolution())(List.empty).evolution
    new Evolution[Point] {
      override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] =
        config.run[Id, Evo, EvaluationResult](new ToEvolution[Evo](alg)).get(List.empty)
    }
  }
  val initialConfig: Config = new Config {
    override def run[S[_], F[_], R[_]](alg: DrawingAlgebra[S, F, R, String]): R[F[Point]] = {
      import alg.drawing._, alg.bind._
      import alg.scalar._
      fix(lambda("self", cons(point(0, 0), var0[F[Point]])))
    }
  }

  override def configCodec: JsonCodec[Config] =
    DrawingJsonCodec
}
