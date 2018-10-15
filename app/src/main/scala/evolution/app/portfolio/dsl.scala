package evolution.app.portfolio

import cats.Id
import cats.kernel.Semigroup
import evolution.algebra._
import evolution.app.codec.JsonCodec
import evolution.app.codec.config.DrawingJsonCodec
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent.instance
import evolution.app.react.component.config.{ConfigComponent, instances}
import evolution.geometry.Point
import evolution.primitive.algebra
import evolution.primitive.algebra.binding.interpreter.EvaluationResult
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.evolution.interpreter.{EvolutionAlgebraEvaluator, EvolutionAlgebraSerializer}
import evolution.primitive.algebra.evolution.parser.EvolutionAlgebraGrammar
import fastparse.noApi
import japgolly.scalajs.react.vdom.html_<^._

object dsl extends DrawingDefinition[Point] {
  val name = "drawing dsl"

  trait Config {
    def run[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String]): R[F[Point]]
  }

  override val configComponent: ConfigComponent[Config] = {
    // TODO improve this rubbish
    instance[Config]("drawing config") { (config2Snapshot, children) =>
      val stringSnapshot =
        config2Snapshot.zoomState[String](config2 => config2.run(EvolutionAlgebraSerializer)(Nil)) {
          serialized => previousConfig =>
            new Config {
              override def run[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String]): R[F[Point]] = {
                val grammar = EvolutionAlgebraGrammar.grammar[S, F, R](alg)
                val parser: noApi.Parser[R[F[Point]]] =
                  grammar.list.evolutionOf(grammar.constants.points)(Semigroup[Point])(Nil)
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

  def evolution(config: Config, context: DrawingContext): LegacyEvolution[Point] = {
    new LegacyEvolution[Point] {
      override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] =
        config.run[Id, Evo, EvaluationResult](new EvolutionAlgebraEvaluator[Evo](alg)).get(List.empty)
    }
  }
  val initialConfig: Config = new Config {
    override def run[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String]): R[F[Point]] = {
      import alg.list._, alg.bind._
      import alg.constants._
      fix(lambda("self", cons(point(double(0), double(0)), var0[F[Point]])))
    }
  }

  override def configCodec: JsonCodec[Config] =
    DrawingJsonCodec
}
