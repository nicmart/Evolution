package evolution.primitive
import evolution.primitive.algebra.evolution.Evolution

class FullModule[F[_]] extends ParsersModule[F] with TyperModule[F] with CompilerModule[F] with WithAst[F] {
  import ast._
  def parse[R[_]](
    serialisedExpr: String,
    expectedType: Type,
    alg: Evolution[F, R],
    ctx: VarContext
  ): Either[String, R[expectedType.Out]] = {

    println("Start Compilation")

    val parsed: Either[String, Expr] =
      Parsers.parser
        .parse(serialisedExpr)
        .fold((_, failIndex, extra) => Left(s"Failed at $failIndex: ${extra.traced.trace}"), (expr, _) => Right(expr))

    for {
      expr <- parsed
      _ = println("Done: Parsing of AST")
      (exprWithTypeVars, constraints) = Typer.assignVarsAndFindConstraints(expr)
      _ = println("Done: Constraints generation")
      constraintsWithExpectedType = constraints.merge(Typer.Constraints(expectedType -> exprWithTypeVars.tpe))
      unification <- Typer.unify(constraintsWithExpectedType)
      _ = println("Done: unification")
      typedExpr = unification.substitute(exprWithTypeVars)
      _ = println("Done: substitution")
      result <- Compiler.compile[R](typedExpr, alg, ctx)
      _ = println("Done: compilation")
    } yield result.asInstanceOf[R[expectedType.Out]]
  }
}
