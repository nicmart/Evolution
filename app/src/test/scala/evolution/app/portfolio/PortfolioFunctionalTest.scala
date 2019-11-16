package evolution.app.model.component

import evolution.compiler.LanguageSpec
import evolution.app.portfolio.Portfolio
import evolution.app.conf.Conf
import evolution.app.model.context.DrawingContext
import evolution.app.model.context.DrawingContext.CanvasSize
import evolution.app.model.CodeCompiler
import evolution.compiler.phases.FullCompiler
import evolution.compiler.impl.evaluation.EvalMaterializer
import evolution.compiler.phases.parser.FastParseParser
import evolution.compiler.phases.compiler.DefaultCompiler
import evolution.compiler.phases.typer._
import evolution.compiler.phases.typer.predicates.UnifyPredicates

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

          result.unsafeRight shouldBe a[Iterator[_]]
          result.unsafeRight.take(100).toList should have length (100)
        }
      }
    }
  }

  lazy val parser = FastParseParser
  //lazy val typer = new UnificationTyper(Conf.logger)
  lazy val typer = new PredicatesSolverTyper(new RecursiveTyper, new UnifyPredicates(Conf.logger))
  lazy val compiler = DefaultCompiler
  lazy val materializer = EvalMaterializer
  //lazy val compiler = new CodeCompiler(new FullCompiler(typedTreeCompiler, JsCodeMaterializer, Conf.logger))
  lazy val codeCompiler = new CodeCompiler(new FullCompiler(parser, typer, compiler, materializer, Conf.logger))
}
