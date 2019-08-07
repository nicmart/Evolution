package evolution.language
import cats.implicits._
import cats.mtl.implicits._
import evolution.compiler.ast.AST
import evolution.data.Expr
import evolution.compiler.phases.parsing.Parser
import evolution.compiler.phases.typing.{ Constraints, TypeInference, AssignFreshTypeVars }
import evolution.compiler.phases.typing.TypeInference.TypeInferenceInstances._
import evolution.compiler.types.Type

object FullModule {

  def parse[M[_]](
    serialisedExpr: String,
    expectedType: Type,
    ctx: VarContext
  )(implicit M: TypeInference[M]): M[Expr[expectedType.Out]] = {

    println("Start Compilation")

    val parsed: M[AST] =
      Parser
        .parse(serialisedExpr)
        .fold(
          _.message.raise[M, AST],
          _.pure[M]
        )

    for {
      expr <- parsed
      _ = println("Done: Parsing of AST")
      exprWithTypeVars <- AssignFreshTypeVars.assign(expr)
      constraints <- Typer.findConstraints(exprWithTypeVars)
      _ = println("Done: Constraints generation")
      constraintsWithExpectedType = constraints.merge(Constraints(expectedType -> exprWithTypeVars.tpe.t))
      unification <- Typer.unify[M](constraintsWithExpectedType)
      _ = println("Done: unification")
      start = System.currentTimeMillis()
      predicateSubst <- Typer.predicatesSubstitution(unification.substitutedPredicates)
      stop = System.currentTimeMillis()
      _ = println(s"Predicate unification time: ${(stop - start)}")
      subst = predicateSubst.compose(unification.substitution)
      typedExpr = subst.substitute(exprWithTypeVars)
      _ = println("Done: substitution")
      _ = println(s"Typed expression: $typedExpr")
      result <- Compiler.compile[M](typedExpr).run(ctx)
      _ = println(s"Compiled to $result")
      _ = println("Done: compilation")
    } yield result.asInstanceOf[Expr[expectedType.Out]]
  }

}
