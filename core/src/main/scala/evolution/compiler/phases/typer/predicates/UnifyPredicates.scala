package evolution.compiler.phases.typer.predicates

import evolution.compiler.phases.typer.model.Substitution
import evolution.compiler.phases.typer.predicates.model.PredicateConditions
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Predicate
import evolution.logging.Logger

final class UnifyPredicates(logger: Logger) {

  import logger.log

  def unify(instances: List[Predicate], predicates: List[Predicate]): Either[String, Substitution] = {
    log("Unique Predicates:")
    log(predicates.distinct)

    val predicateConditions: List[PredicateConditions] =
      logTime("Compute Substitutions") {
        predicates.distinct.map { predicate =>
          PredicateConditions(
            predicate,
            instances.flatMap(instance => matchPredicateWithInstance(instance, predicate)).toSet
          )
        }
      }

    logTime("Compute Solution") {
      predicateConditions match {
        case Nil          => Right(Substitution.empty)
        case head :: tail => PredicatesSolver(head, tail.toVector).solve
      }
    }
  }

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

  private def logTime[T](message: String)(t: => T): T = {
    val start = System.currentTimeMillis()
    val result = t
    val stop = System.currentTimeMillis()
    log(s"Time to $message: ${stop - start}ms")
    result
  }
}
