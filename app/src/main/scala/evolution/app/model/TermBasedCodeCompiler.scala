package evolution.app.model

import evolution.app.model.context.DrawingContext
import evolution.compiler.phases.FullCompiler
import evolution.compiler.phases.typer.config.ConstConfig
import evolution.compiler.phases.typer.model.{Assumption, Assumptions}
import evolution.compiler.stdlib.StandardLibraryModule
import evolution.compiler.term.Term.Literal
import evolution.compiler.term.{Module, Term}
import evolution.compiler.types.Type
import evolution.geometry.Point
import evolution.materialization.Evolution

final class TermBasedCodeCompiler(fullCompiler: FullCompiler) extends CodeCompiler {
  def compile(code: String, seed: Long, ctx: DrawingContext): Either[String, Iterator[Point]] =
    module(ctx).flatMap(
      mod =>
        fullCompiler
          .compile(
            code,
            Type.Evo(Type.Point),
            mod
          )
          .map { evolution =>
            Evolution.runWithSeed(seed, evolution.asInstanceOf[Evolution[Point]])
          }
    )

  private val assumptions = Assumptions(
    ConstConfig.constants.map(constant => Assumption(constant.name, constant.tpe, false))
  )

  private def module(ctx: DrawingContext): Either[String, Module] =
    StandardLibraryModule.module.map(Module(assumptions, loadVars(ctx)).compose)

  private def loadVars(ctx: DrawingContext)(term: Term): Term = {
    val vars = List(
      "top" -> Term.Lit(Literal.LitDouble(ctx.top)),
      "bottom" -> Term.Lit(Literal.LitDouble(ctx.bottom)),
      "left" -> Term.Lit(Literal.LitDouble(ctx.left)),
      "right" -> Term.Lit(Literal.LitDouble(ctx.right))
    )

    vars.foldLeft(term) { case (accExpr, (varname, varbody)) => Term.Let(varname, varbody, accExpr) }
  }
}
