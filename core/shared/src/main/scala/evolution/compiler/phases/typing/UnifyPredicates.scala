package evolution.compiler.phases.typing

import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Predicate
import cats.implicits._
import evolution.compiler.phases.typing.model.Substitution

object UnifyPredicates {

  def unify(instances: List[Predicate], predicates: List[Predicate]): Either[String, Substitution] = {
    val predicateToSubstitutions: Map[Predicate, List[Substitution]] =
      predicates.distinct.map { predicate =>
        predicate ->
          instances.flatMap(instance => matchPredicateWithInstance(instance, predicate))
      }.toMap

    val reducedPredicateToSubstitutions = predicateToSubstitutions.filterNot {
      case (_, substitutions) => hasEmptySubstitution(substitutions)
    }

    val orderedSubstitutions = reducedPredicateToSubstitutions.toList.sortBy {
      case (pred, _) => freeVarsInPredicate(pred)
    }.map(_._2)

    val optimisedSubstitutions = reduceSubstitutions(orderedSubstitutions)

    println("Going to unify predicates:")
    println(reducedPredicateToSubstitutions.mapValues(_.mkString("\n")).mkString("\n"))
    println("Optimised substitutions:")
    println(optimisedSubstitutions.mkString("\n"))

    val combinations = product(optimisedSubstitutions)

    combinations
      .flatMap(mergeSubstitutions)
      .headOption
      .toRight(s"Not able to unify predicates:\n${predicates.distinct.mkString("\n")}")
  }

  private def reduceSubstitutions(substitutions: List[List[Substitution]]): List[List[Substitution]] =
    substitutions match {
      case head :: tail => head :: reduceSubstitutions(tail.map(removeImpossibleSubstitutions(head)))
      case Nil          => Nil
    }

  // Remove substitutions from candidates that are not compatible with any of the required substitutions
  private def removeImpossibleSubstitutions(required: List[Substitution])(
    candidates: List[Substitution]
  ): List[Substitution] = candidates.filter(atLeastOneCompatible(required))

  private def atLeastOneCompatible(atLeastOneOf: List[Substitution])(candidate: Substitution): Boolean =
    atLeastOneOf.exists(s => s.merge(candidate).isRight)

  private def freeVarsInPredicate(predicate: Predicate): Int =
    predicate.types.map(tpe => tpe.typeVars.size).sum

  private def hasEmptySubstitution(substitutions: List[Substitution]): Boolean =
    substitutions.contains(Substitution.empty)

  private def matchPredicateWithInstance(instance: Predicate, predicate: Predicate): Option[Substitution] =
    (instance, predicate) match {
      case (Predicate(iId, iTypes), Predicate(pId, pTypes)) if iId == pId => matchTypes(iTypes, pTypes)
      case _                                                              => None
    }

  private def matchTypes(instTypes: List[Type], predTypes: List[Type]): Option[Substitution] =
    (instTypes, predTypes) match {
      case (iHead :: iTail, pHead :: pTail) =>
        for {
          tailSubst <- matchTypes(iTail, pTail)
          headSubst <- matchType(iHead, pHead)
          subst <- headSubst.merge(tailSubst).toOption
        } yield subst

      case (Nil, Nil) => Some(Substitution.empty)
      case _          => None
    }

  private def matchType(instType: Type, predType: Type): Option[Substitution] = (instType, predType) match {
    case (t1, Type.Var(name))         => Some(Substitution(name -> t1))
    case (Type.Evo(t1), Type.Evo(t2)) => matchType(t1, t2)
    case (Type.Lst(t1), Type.Lst(t2)) => matchType(t1, t2)
    case (Type.Arrow(t11, t12), Type.Arrow(t21, t22)) =>
      for {
        s1 <- matchType(t11, t21)
        s2 <- matchType(t12, t22)
        s <- s1.mergeOpt(s2)
      } yield s
    case (t1, t2) if t1 == t2 => Some(Substitution.empty)
    case _                    => None
  }

  private def product[T](lists: List[List[T]]): Stream[List[T]] =
    lists match {
      case firstList :: otherLists =>
        for {
          otherTs <- product(otherLists)
          t <- firstList
        } yield t :: otherTs
      case Nil => Stream(Nil)
    }

  private def mergeSubstitutions(substitutions: List[Substitution]): Option[Substitution] =
    substitutions match {
      case substHead :: substTail =>
        mergeSubstitutions(substTail).flatMap(_.merge(substHead).toOption)
      case Nil => Some(Substitution.empty)
    }
}
