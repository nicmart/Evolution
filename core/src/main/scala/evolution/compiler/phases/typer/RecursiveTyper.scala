package evolution.compiler.phases.typer

import cats.implicits._
import evolution.compiler.phases.Typer
import evolution.compiler.phases.typer.Inference._
import evolution.compiler.phases.typer.RecursiveTyper._
import evolution.compiler.phases.typer.model.{Assignment, Assumption, Assumptions, Substitution}
import evolution.compiler.tree.TreeF.{Bool, DoubleLiteral, Id, IntLiteral, Lambda, Let, Lst}
import evolution.compiler.tree.{Tree, _}
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses._
import evolution.compiler.types._

final class RecursiveTyper extends Typer {

  def typeTree(tree: Tree, expectedType: Option[Type], assumptions: Assumptions): Either[String, TypedTree] =
    typeTreeAndSubstitute(tree, expectedType, assumptions).runA(InferenceState.empty)

  private def typeTreeAndSubstitute(
      tree: Tree,
      expectedType: Option[Type],
      initialAssumptions: Assumptions
  ): Inference[TypedTree] =
    for {
      currentAssumptions <- assumptions
      _ <- setAssumptions(currentAssumptions.merge(initialAssumptions))
      typed <- typeTreeInf(tree)
      _ <- expectedType.fold(InferenceMonad.pure(()))(expected => unify(expected, typed.annotation.value))
      finalSubstitution <- substitution
    } yield finalSubstitution.substitute(typed)

  private def typeTreeInf(tree: Tree): Inference[TypedTree] = {
    tree.value match {
      case Bool(b) => Bool(b).typeWithNoPredicates(Type.Bool).pure[Inference]

      case DoubleLiteral(n) =>
        DoubleLiteral(n).typeWithNoPredicates(Type.Double).pure[Inference]

      case IntLiteral(n) =>
        for {
          typeVar <- newTypeVar
          predicate = Predicate("Num", List(typeVar))
        } yield IntLiteral(n).typeWithSinglePredicate(predicate, typeVar)

      case Id(name, _) =>
        for {
          assumption <- getAssumption(name)
          qualifiedScheme = assumption.qualifiedScheme
          vars = qualifiedScheme.value.vars.map(Type.Var)
          freshTypeVars <- vars.traverse(_ => newTypeVar)
          newQualifiedType = instantiate(qualifiedScheme, freshTypeVars)
        } yield Id(name, assumption.primitive).annotate(newQualifiedType)

      case Lambda(varName, body) =>
        for {
          freshTypeVarname <- newTypeVarname
          assumption = Assumption(varName, Qualified(Scheme(Type.Var(freshTypeVarname))), false)
          typedBody <- withLocalAssumption(assumption)(typeTreeInf(body))
          lambdaType = Type.Var(freshTypeVarname) =>: typedBody.annotation.value
          lambdaPredicates = typedBody.annotation.predicates
          lambdaQualifiedType = qualified(lambdaPredicates, lambdaType)
        } yield Lambda(varName, typedBody).annotate(lambdaQualifiedType)

      case TreeF.App(f, inputs) =>
        for {
          typedF <- typeTreeInf(f)
          typedInputs <- inputs.traverse(tree => typeTreeInf(tree))
          inputTypes = typedInputs.map(_.annotation.value).toList
          inputPredicates = typedInputs.toList.flatMap(_.annotation.predicates)
          returnType <- newTypeVar
          _ <- unify(typedF.annotation.value, arrowType(inputTypes, returnType))
          qualifiedType = qualified(typedF.annotation.predicates ++ inputPredicates, returnType)
        } yield TreeF.App(typedF, typedInputs).annotate(qualifiedType)

      case Lst(ts) =>
        for {
          typedTs <- ts.traverse(tree => typeTreeInf(tree))
          freshTypeVar <- newTypeVar
          _ <- typedTs.traverse(typedTree => unify(freshTypeVar, typedTree.annotation.value))
          qualifiedType = qualified(typedTs.flatMap(_.annotation.predicates), Type.Lst(freshTypeVar))
        } yield Lst(typedTs).annotate(qualifiedType)

      case Let(varName, expr, in) =>
        for {
          currentAssumptions <- assumptions
          typedExpr <- typeTreeInf(expr)
          currentSubst <- substitution
          assumption = Assumption(
            varName,
            // We need to apply the substitution before quantifying
            // Otherwise we quantify things like Mult(T0, T1, T5) => T2 -> T3 -> T4
            // when T0 -> T2, T1 -> T3, T5 -> T4
            quantify(currentSubst.substitute(typedExpr.annotation), currentAssumptions),
            false
          )
          typedIn <- withLocalAssumption(assumption)(typeTreeInf(in))
          letPredicates = typedIn.annotation.predicates
          //letPredicates = typedIn.annotation.predicates ++ typedExpr.annotation.predicates
          letType = qualified(letPredicates, typedIn.annotation.value)
        } yield Let(varName, typedExpr, typedIn).annotate(letType)
    }
  }
}

object RecursiveTyper {
  private def qualified(predicates: List[Predicate], tpe: Type): Qualified[Type] =
    Qualified(predicates.distinct, tpe)

  private def quantify(qualified: Qualified[Type], assumptions: Assumptions): Qualified[Scheme] = {
    val vars = (qualified.value.typeVars.map(_.name) ++ qualified.predicatesTypeVars).diff(assumptions.allFreeTypeVars)
    Qualified(qualified.predicates, Scheme(vars.toList, qualified.value))
  }

//  private def quantify(qualified: Qualified[Type], assumptions: Assumptions): Qualified[Scheme] =
//    qualified.map(Scheme.apply)

  private def instantiate(qs: Qualified[Scheme], types: List[Type]): Qualified[Type] = {
    val assignments =
      qs.value.vars.zip(types).map { case (from, to) => Assignment(from, to) }
    val substitution = Substitution(assignments)
    Qualified(
      substitution.substitute(qs.predicates),
      qs.value.instantiate(types)
    )
  }

  def arrowType(inputs: List[Type], result: Type): Type =
    inputs.foldRight(result)(_ =>: _)

  implicit class LeafOps(tree: TreeF[Nothing]) {
    def typed: TreeF[TypedTree] = tree
    def typeWithNoPredicates(tpe: Type): TypedTree =
      typed.annotate(Qualified(tpe))
    def typeWithSinglePredicate(predicate: Predicate, tpe: Type): TypedTree =
      typed.annotate(Qualified(List(predicate), tpe))
  }
}
