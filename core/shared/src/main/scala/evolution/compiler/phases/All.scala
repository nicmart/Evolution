package evolution.compiler.phases

import cats.implicits._
import cats.mtl.implicits._
import evolution.compiler.ast.AST
import evolution.data.Expr
import evolution.compiler.phases.parsing.Parser
import evolution.compiler.phases.typing._
import evolution.compiler.phases.compiling._
import evolution.compiler.phases.typing.model.{ Constraints, TypeInference }
import TypeInference.TypeInferenceInstances._
import evolution.compiler.phases.compiling.model.VarContext
import evolution.compiler.types.Type
import evolution.compiler.types.TypeBindings

object All {

  def compile[M[_]](
    serialisedExpr: String,
    expectedType: Type,
    typeBindings: TypeBindings,
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
      exprWithTypeVars <- AssignFreshTypeVars.assign(expr, typeBindings)
      constraints <- FindConstraints.find(exprWithTypeVars)
      _ = println("Done: Constraints generation")
      constraintsWithExpectedType = constraints.merge(Constraints(expectedType -> exprWithTypeVars.tpe.t))
      unification <- UnifyTypes.unify[M](constraintsWithExpectedType)
      _ = println("Done: unification")
      start = System.currentTimeMillis()
      predicateSubst <- UnifyPredicates.unifyM[M](unification.substitutedPredicates)
      stop = System.currentTimeMillis()
      _ = println(s"Predicate unification time: ${(stop - start)}")
      subst = predicateSubst.compose(unification.substitution)
      typedExpr = subst.substitute(exprWithTypeVars)
      _ = println("Done: substitution")
      _ = println(s"Typed expression: $typedExpr")
      result <- Compile.compile[M](typedExpr).run(ctx)
      _ = println(s"Compiled to $result")
      _ = println("Done: compilation")
    } yield result.asInstanceOf[Expr[expectedType.Out]]
  }

}
