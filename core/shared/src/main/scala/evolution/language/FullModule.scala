package evolution.language
import cats.MonadError
import cats.implicits._

class FullModule[F[_]] extends ParsersModule[F] with TyperModule[F] with CompilerModule[F] with ASTModule[F] {

  def parse[R[_]](
    serialisedExpr: String,
    expectedType: Type,
    ctx: VarContext
  ): Either[String, R[expectedType.Out]] = {

    println("Start Compilation")

    val parsed: Either[String, AST] =
      Parsers.parser
        .parse(serialisedExpr)
        .fold((_, failIndex, extra) => Left(s"Failed at $failIndex: ${extra.traced.trace}"), (expr, _) => Right(expr))

    for {
      expr <- parsed
      _ = println("Done: Parsing of AST")
      (exprWithTypeVars, constraints) = Typer.assignVarsAndFindConstraints(expr).evaluate
      _ = println("Done: Constraints generation")
      constraintsWithExpectedType = constraints.merge(Typer.Constraints(expectedType -> exprWithTypeVars.tpe))
      unification <- Typer.unify(constraintsWithExpectedType)
      _ = println("Done: unification")
      _ <- Typer.checkPredicates(unification.substitutedPredicates)
      _ = println(s"Checked Predicates: ${unification.substitutedPredicates} on $unification")
      typedExpr = unification.substitution.substitute(exprWithTypeVars)
      _ = println("Done: substitution")
      result <- Compiler.compile[Either[String, ?]](typedExpr).run(ctx)
      _ = println("Done: compilation")
    } yield result.asInstanceOf[R[expectedType.Out]]
  }

}