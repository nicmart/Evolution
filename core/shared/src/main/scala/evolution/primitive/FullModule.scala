package evolution.primitive
import evolution.primitive.algebra.evolution.Evolution

class FullModule[F[_]] extends ParsersModule[F] with TyperModule[F] with CompilerModule[F] with WithAst[F] {
  import ast._
  def parse[R[_]](
    serialisedExpr: String,
    expectedType: Type,
    alg: Evolution[F, R]): Either[String, R[expectedType.Out]] = {

    val parsed: Either[String, Expr] =
      Parsers.parser.parse(serialisedExpr).fold((_, _, f) => Left(f.toString), (expr, _) => Right(expr))

    for {
      expr <- parsed
      (exprWithTypeVars, constraints) = Typer.assignVarsAndFindConstraints(expr)
      constraintsWithExpectedType = constraints.merge(Typer.Constraints(expectedType -> exprWithTypeVars.tpe))
      unification <- Typer.unify(constraintsWithExpectedType)
      typedExpr = unification.substitute(exprWithTypeVars)
      result <- Compiler.compile[R](typedExpr, alg)
    } yield result.asInstanceOf[R[expectedType.Out]]
  }
}
