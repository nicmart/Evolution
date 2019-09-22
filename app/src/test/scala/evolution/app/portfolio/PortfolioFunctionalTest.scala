package evolution.app.model.component

import evolution.compiler.LanguageSpec
import evolution.app.portfolio.Portfolio
import evolution.app.conf.Conf
import evolution.app.model.context.DrawingContext
import evolution.app.model.context.DrawingContext.CanvasSize
import evolution.app.model.CodeCompiler
import evolution.compiler.phases.AllPhases
import evolution.compiler.impl.jsmaterialization.JsCodeMaterializer

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
        }
      }
    }
  }

  lazy val compiler = new CodeCompiler(new AllPhases(JsCodeMaterializer, Conf.logger))
}
