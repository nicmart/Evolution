package evolution.language
import cats.implicits._
import cats.mtl.implicits._

class FullModule[F[_]]
    extends ParserModule[F]
    with CompilerModule[F]
    with TyperModule[F]
    with ASTModule[F]
    with PredefinedConstantsModule[F] {

  def parse[R[_], M[_]](
    serialisedExpr: String,
    expectedType: Type,
    ctx: VarContext
  )(implicit M: Typer.TypeInference[M]): M[R[expectedType.Out]] = {

    import Typer.TypeInferenceInstances._

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
      exprAndConstraints <- Typer.assignVarsAndFindConstraints(expr)
      (exprWithTypeVars, constraints) = exprAndConstraints
      _ = println("Done: Constraints generation")
      constraintsWithExpectedType = constraints.merge(Typer.Constraints(expectedType -> exprWithTypeVars.tpe.t))
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
    } yield result.asInstanceOf[R[expectedType.Out]]
  }

}
