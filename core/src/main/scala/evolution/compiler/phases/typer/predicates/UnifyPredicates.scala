package evolution.compiler.phases.typer.predicates

import evolution.compiler.phases.typer.Matchable.*
import evolution.compiler.phases.typer.model.Substitution
import evolution.compiler.phases.typer.predicates.model.PredicateConditions
import evolution.compiler.types.TypeClasses.Predicate
import evolution.logging.Logger

import scala.collection.immutable.ListSet

class UnifyPredicates(logger: Logger):

  import logger.log

  def unify(instances: List[Predicate], predicates: List[Predicate]): Either[String, Substitution] =
    log("Unique Predicates:")
    log(predicates.distinct)

    val predicateConditions: List[PredicateConditions] =
      logTime("Compute Substitutions") {
        predicates.distinct.map { predicate =>
          PredicateConditions(
            predicate,
            ListSet.from(instances.flatMap(instance => tryMatch(instance, predicate)))
          )
        }
      }

    logTime("Compute Solution") {
      predicateConditions match
        case Nil          => Right(Substitution.empty)
        case head :: tail => PredicatesSolver(head, tail.toVector).solve
    }

  private def logTime[T](message: String)(t: => T): T =
    val start = System.currentTimeMillis()
    val result = t
    val stop = System.currentTimeMillis()
    log(s"Time to $message: ${stop - start}ms")
    result
