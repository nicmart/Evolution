package evolution.compiler.phases

import cats.implicits._
import evolution.compiler.ast.AST
import evolution.compiler.phases.parsing.Parser
import evolution.compiler.phases.typing._
import evolution.compiler.phases.compiling._
import evolution.compiler.phases.typing.model.Constraints
import evolution.compiler.phases.compiling.model.VarContext
import evolution.compiler.types.Type
import evolution.compiler.types.TypeBindings
import evolution.compiler.phases.typing.config.TypingConfig
import scala.collection.immutable.Nil
import evolution.materialization.Evolution
import evolution.geometry.Point
import evolution.compiler.phases.materializing.Materialize
import evolution.data.emptyCtx
import evolution.logging.Logger

class AllPhases(logger: Logger) {
  import logger.log

  // TODO here we are assuming the the expected type can be anything, but that the output is Evolution[Point]???
  def compile(
    serialisedExpr: String,
    expectedType: Type,
    typeBindings: TypeBindings,
    varBindings: List[(String, AST)]
  ): Either[String, Evolution[Point]] =
    for {
      ast <- Parser.parse(serialisedExpr).leftMap(_.message)
      _ = log("Done: Parsing of AST")
      astWithVars = addVars(ast, varBindings)
      astWithTypeVars <- AssignFreshTypeVars.assign(astWithVars, typeBindings).asRight
      _ = log(s"Un-typed expression:")
      _ = log(AST.prettyPrint(astWithTypeVars))
      constraints <- FindConstraints.find(astWithTypeVars)
      _ = log("Done: Constraints generation")
      constraintsWithExpectedType = constraints.merge(Constraints(expectedType -> astWithTypeVars.tpe.t))
      unification <- UnifyTypes.unify(constraintsWithExpectedType)
      _ = log("Done: unification")
      _ = log(s"Partially typed AST:")
      _ = log(AST.prettyPrint(unification.substitution.substitute(astWithTypeVars)))
      start = System.currentTimeMillis()
      predicateSubst <- new UnifyPredicates(logger).unify(TypingConfig.instances, unification.substitutedPredicates)
      stop = System.currentTimeMillis()
      _ = log(s"Predicate unification time: ${(stop - start)}")
      subst = predicateSubst.compose(unification.substitution)
      typedAst = subst.substitute(astWithTypeVars)
      _ = log("Done: substitution")
      _ = log(s"Typed expression:")
      _ = log(AST.prettyPrint(typedAst))
      expression <- Compile.compile(typedAst, varContext(varBindings))
      _ = log(s"Compiled to $expression")
      _ = log("Done: compilation")
      evolution = Materialize.materialize(expression).apply(emptyCtx)
      _ = log(s"Materialized to $evolution")
    } yield evolution.asInstanceOf[Evolution[Point]]

  private def varContext(varBindings: List[(String, AST)]): VarContext =
    new VarContext(varBindings.map(_._1))

  private def addVars(ast: AST, varBindings: List[(String, AST)]): AST = varBindings match {
    case (name, value) :: tl => AST.Let(name, value, addVars(ast, tl))
    case Nil                 => ast
  }
}
