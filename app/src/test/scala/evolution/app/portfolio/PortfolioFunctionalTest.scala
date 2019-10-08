package evolution.app.model.component

import evolution.compiler.LanguageSpec
import evolution.app.portfolio.Portfolio
import evolution.app.conf.Conf
import evolution.app.model.context.DrawingContext
import evolution.app.model.context.DrawingContext.CanvasSize
import evolution.app.model.CodeCompiler
import evolution.compiler.phases.FullCompiler
import evolution.compiler.impl.jsmaterialization.JsCodeMaterializer
import evolution.compiler.phases.TypedTreeCompiler
import evolution.compiler.impl.evaluation.EvalMaterializer

class PortfolioSpec extends LanguageSpec {
  "Drawings in Portfolio" - {
    "should compile" - {
      Portfolio.drawings.foreach { drawing =>
        drawing.title.getOrElse("untitled") in {
          val result =
            compiler.compile(
              drawing.drawingState.code,
              drawing.drawingState.seed,
              DrawingContext(CanvasSize(200, 200))
            )

          result.unsafeEvaluate shouldBe a[Iterator[_]]
          result.unsafeEvaluate.take(100).toList should have length (100)
        }
      }
    }
  }

  lazy val typedTreeCompiler = new TypedTreeCompiler(Conf.logger)
  //lazy val compiler = new CodeCompiler(new FullCompiler(typedTreeCompiler, JsCodeMaterializer, Conf.logger))
  lazy val compiler = new CodeCompiler(new FullCompiler(typedTreeCompiler, EvalMaterializer, Conf.logger))
}
