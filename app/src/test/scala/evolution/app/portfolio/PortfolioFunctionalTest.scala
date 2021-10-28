package evolution.app.model.component

import evolution.app.conf.Conf
import evolution.app.model.TermBasedCodeCompiler
import evolution.app.model.context.DrawingContext
import evolution.app.model.context.DrawingContext.CanvasSize
import evolution.app.portfolio.Portfolio
import evolution.compiler.LanguageSpec
import evolution.compiler.phases.FullCompiler
import evolution.compiler.phases.parser.CatsParseParser
import evolution.compiler.phases.typer._
import evolution.compiler.phases.typer.predicates.UnifyPredicates
import evolution.compiler.term.{RegisterBasedInterpreter, TreeToTermCompiler}

class PortfolioFunctionalTest extends LanguageSpec:
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

          result.unsafeRight shouldBe a[Iterator[?]]
          result.unsafeRight.take(100).toList should have length (100)
        }
      }
    }
  }

  /*  val constants = TypingConfig.constantQualifiedTypes.all.values
    .map(assumption => Const(assumption.name, assumption.qualifiedScheme, ""))
    .toList

  lazy val printer: PPrinter = pprint.PPrinter.BlackWhite.copy(
    additionalHandlers = {
      case Type.Arrow(from @ Type.Arrow(a, b), to) =>
        Tree.Infix(Tree.Apply("", List(printer.treeify(from)).toIterator), "=>:", printer.treeify(to))
      case Type.Arrow(from, to) =>
        Tree.Infix(printer.treeify(from), "=>:", printer.treeify(to))
    }
  )

  printer.pprintln(constants, height = Int.MaxValue)*/

  lazy val parser = CatsParseParser
  //lazy val typer = new UnificationTyper(Conf.logger)
  lazy val typer = new PredicatesSolverTyper(new RecursiveTyper, new UnifyPredicates(Conf.logger))
  lazy val compiler = new TreeToTermCompiler
  lazy val interpreter = new RegisterBasedInterpreter
  //lazy val compiler = new CodeCompiler(new FullCompiler(typedTreeCompiler, JsCodeMaterializer, Conf.logger))
  lazy val codeCompiler = new TermBasedCodeCompiler(
    new FullCompiler(parser, typer, compiler, interpreter, Conf.logger)
  )
