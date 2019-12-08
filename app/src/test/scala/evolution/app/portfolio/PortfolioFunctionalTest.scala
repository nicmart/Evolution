package evolution.app.model.component

import evolution.app.conf.Conf
import evolution.app.model.TermBasedCodeCompiler
import evolution.app.model.context.DrawingContext
import evolution.app.model.context.DrawingContext.CanvasSize
import evolution.app.portfolio.Portfolio
import evolution.compiler.LanguageSpec
import evolution.compiler.phases.FullCompiler
import evolution.compiler.phases.parser.FastParseParser
import evolution.compiler.phases.typer._
import evolution.compiler.phases.typer.predicates.UnifyPredicates
import evolution.compiler.term.{TermInterpreter, TreeToTermCompiler}

class PortfolioFunctionalTest extends LanguageSpec {
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
  lazy val compiler = new TreeToTermCompiler
  lazy val interpreter = new TermInterpreter
  //lazy val compiler = new CodeCompiler(new FullCompiler(typedTreeCompiler, JsCodeMaterializer, Conf.logger))
  lazy val codeCompiler = new TermBasedCodeCompiler(
    new FullCompiler(parser, typer, compiler, interpreter, Conf.logger)
  )
}
