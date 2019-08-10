package evolution.compiler.phases

import cats.implicits._
import evolution.compiler.ast.AST
import evolution.data.Expr
import evolution.compiler.phases.parsing.Parser
import evolution.compiler.phases.typing._
import evolution.compiler.phases.compiling._
import evolution.compiler.phases.typing.model.Constraints
import evolution.compiler.phases.compiling.model.VarContext
import evolution.compiler.types.Type
import evolution.compiler.types.TypeBindings
import evolution.compiler.phases.typing.config.TypingConfig

object All {

  def compile(
    serialisedExpr: String,
    expectedType: Type,
    typeBindings: TypeBindings,
    ctx: VarContext
  ): Either[String, Expr[expectedType.Out]] =
    for {
      expr <- Parser.parse(serialisedExpr).leftMap(_.message)
      _ = println("Done: Parsing of AST")
      exprWithTypeVars <- AssignFreshTypeVars.assign(expr, typeBindings).asRight
      constraints <- FindConstraints.find(exprWithTypeVars)
      _ = println("Done: Constraints generation")
      constraintsWithExpectedType = constraints.merge(Constraints(expectedType -> exprWithTypeVars.tpe.t))
      unification <- UnifyTypes.unify(constraintsWithExpectedType)
      _ = println("Done: unification")
      start = System.currentTimeMillis()
      predicateSubst <- UnifyPredicates.unify(TypingConfig.instances, unification.substitutedPredicates)
      stop = System.currentTimeMillis()
      _ = println(s"Predicate unification time: ${(stop - start)}")
      subst = predicateSubst.compose(unification.substitution)
      typedExpr = subst.substitute(exprWithTypeVars)
      _ = println("Done: substitution")
      _ = println(s"Typed expression:")
      _ = println(AST.prettyPrint(typedExpr))
      result <- Compile.compile(typedExpr, ctx)
      _ = println(s"Compiled to $result")
      _ = println("Done: compilation")
    } yield result.asInstanceOf[Expr[expectedType.Out]]
}
