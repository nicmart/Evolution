package evolution.app.model.component

import evolution.compiler.LanguageSpec
import evolution.app.portfolio.Portfolio
import evolution.app.conf.Conf
import evolution.app.model.context.DrawingContext
import evolution.app.model.context.DrawingContext.CanvasSize
import evolution.app.model.CodeCompiler
import evolution.compiler.phases.FullCompiler
import evolution.compiler.impl.jsmaterialization.JsCodeMaterializer
import evolution.compiler.impl.evaluation.EvalMaterializer
import evolution.compiler.phases.typing.UnificationTyper
import evolution.compiler.phases.parsing.FastParseParser
import evolution.compiler.phases.compiling.DefaultCompiler

class PortfolioSpec extends LanguageSpec {
  "Drawings in Portfolio" - {
    "should compile" - {
      Portfolio.drawings.foreach { drawing =>
        drawing.title.getOrElse("untitled") in {
          val result =
            codeCompiler.compile(
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

  lazy val parser = FastParseParser
  lazy val typer = new UnificationTyper(Conf.logger)
  lazy val compiler = DefaultCompiler
  lazy val materializer = EvalMaterializer
  //lazy val compiler = new CodeCompiler(new FullCompiler(typedTreeCompiler, JsCodeMaterializer, Conf.logger))
  lazy val codeCompiler = new CodeCompiler(new FullCompiler(parser, typer, compiler, materializer, Conf.logger))
}
