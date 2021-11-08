package evolution.compiler.phases.typer

import cats.implicits.*
import evolution.compiler.phases.Typer
import evolution.compiler.phases.typer.Inference.*
import evolution.compiler.phases.typer.RecursiveTyper.*
import evolution.compiler.phases.typer.model.{Assignment, Assumption, Assumptions, Substitution}
import evolution.compiler.tree.TreeF.{Bool, DoubleLiteral, Id, IntLiteral, Lambda, Let, Lst}
import evolution.compiler.tree.{Tree, *}
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.*
import evolution.compiler.types.*

final class RecursiveTyper extends Typer:

  def typeTree(tree: Tree, expectedType: Option[Type], assumptions: Assumptions): Either[String, TypedTree] =
    typeTreeAndSubstitute(tree, expectedType, assumptions).runA(InferenceState.empty)

  private def typeTreeAndSubstitute(
      tree: Tree,
      expectedType: Option[Type],
      initialAssumptions: Assumptions
  ): Inference[TypedTree] =
    for
      currentAssumptions <- assumptions
      _ <- setAssumptions(currentAssumptions.merge(initialAssumptions))
      typed <- typeTreeInf(tree)
      _ <- expectedType.fold(().pure[Inference])(expected => unify(expected, typed.annotation.value))
      finalSubstitution <- substitution
    yield finalSubstitution.substitute(typed)

  private def typeTreeInf(tree: Tree): Inference[TypedTree] =
    tree.value match
      case Bool(b, pos) => Bool(b, pos).typeWithNoPredicates(Type.Bool).pure[Inference]

      case DoubleLiteral(n, pos) =>
        DoubleLiteral(n, pos).typeWithNoPredicates(Type.Double).pure[Inference]

      case IntLiteral(n, pos) =>
        for
          typeVar <- newTypeVar
          predicate = Predicate("Num", List(typeVar))
        yield IntLiteral(n, pos).typeWithSinglePredicate(predicate, typeVar)

      case Id(name, pos) =>
        for
          assumption <- getAssumption(name.string)
          qualifiedScheme = assumption.qualifiedScheme
          vars = qualifiedScheme.value.vars.map(Type.Var.apply)
          freshTypeVars <- vars.traverse(_ => newTypeVar)
          newQualifiedType = instantiate(qualifiedScheme, freshTypeVars)
        yield Id(name, pos).annotate(newQualifiedType)

      case Lambda(varName, body, pos) =>
        for
          freshTypeVarname <- newTypeVarname
          assumption = Assumption(varName, Qualified(Scheme(Type.Var(freshTypeVarname))))
          typedBody <- withLocalAssumption(assumption)(typeTreeInf(body))
          lambdaType = Type.Var(freshTypeVarname) =>: typedBody.annotation.value
          lambdaPredicates = typedBody.annotation.predicates
          lambdaQualifiedType = qualified(lambdaPredicates, lambdaType)
        yield Lambda(varName, typedBody, pos).annotate(lambdaQualifiedType)

      case TreeF.App(f, inputs, pos) =>
        for
          typedF <- typeTreeInf(f)
          typedInputs <- inputs.traverse(tree => typeTreeInf(tree))
          inputTypes = typedInputs.map(_.annotation.value).toList
          inputPredicates = typedInputs.toList.flatMap(_.annotation.predicates)
          returnType <- newTypeVar
          _ <- unify(typedF.annotation.value, arrowType(inputTypes, returnType))
          qualifiedType = qualified(typedF.annotation.predicates ++ inputPredicates, returnType)
        yield TreeF.App(typedF, typedInputs, pos).annotate(qualifiedType)

      case Lst(ts, pos) =>
        for
          typedTs <- ts.traverse(tree => typeTreeInf(tree))
          freshTypeVar <- newTypeVar
          _ <- typedTs.traverse(typedTree => unify(freshTypeVar, typedTree.annotation.value))
          qualifiedType = qualified(typedTs.flatMap(_.annotation.predicates), Type.Lst(freshTypeVar))
        yield Lst(typedTs, pos).annotate(qualifiedType)

      case Let(varName, expr, in, pos) =>
        for
          currentAssumptions <- assumptions
          typedExpr <- typeTreeInf(expr)
          currentSubst <- substitution
          assumption = Assumption(
            varName,
            // We need to apply the substitution before quantifying
            // Otherwise we quantify things like Mult(T0, T1, T5) => T2 -> T3 -> T4
            // when T0 -> T2, T1 -> T3, T5 -> T4
            quantify(currentSubst.substitute(typedExpr.annotation), currentAssumptions)
          )
          typedIn <- withLocalAssumption(assumption)(typeTreeInf(in))
          letPredicates = typedIn.annotation.predicates
          //letPredicates = typedIn.annotation.predicates ++ typedExpr.annotation.predicates
          letType = qualified(letPredicates, typedIn.annotation.value)
        yield Let(varName, typedExpr, typedIn, pos).annotate(letType)

object RecursiveTyper:
  private def qualified(predicates: List[Predicate], tpe: Type): Qualified[Type] =
    Qualified(predicates.distinct, tpe)

  private def quantify(qualified: Qualified[Type], assumptions: Assumptions): Qualified[Scheme] =
    val vars = (qualified.value.typeVars.map(_.name) ++ qualified.predicatesTypeVars).diff(assumptions.allFreeTypeVars)
    Qualified(qualified.predicates, Scheme(vars.toList, qualified.value))

//  private def quantify(qualified: Qualified[Type], assumptions: Assumptions): Qualified[Scheme] =
//    qualified.map(Scheme.apply)

  private def instantiate(qs: Qualified[Scheme], types: List[Type]): Qualified[Type] =
    val assignments =
      qs.value.vars.zip(types).map { case (from, to) => Assignment(from, to) }
    val substitution = Substitution(assignments)
    Qualified(
      substitution.substitute(qs.predicates),
      qs.value.instantiate(types)
    )

  def arrowType(inputs: List[Type], result: Type): Type =
    inputs.foldRight(result)(_ =>: _)

  extension (tree: TreeF[Nothing])
    def typed: TreeF[TypedTree] = tree
    def typeWithNoPredicates(tpe: Type): TypedTree =
      typed.annotate(Qualified(tpe))
    def typeWithSinglePredicate(predicate: Predicate, tpe: Type): TypedTree =
      typed.annotate(Qualified(List(predicate), tpe))
