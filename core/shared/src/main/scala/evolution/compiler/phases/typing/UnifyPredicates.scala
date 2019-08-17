package evolution.compiler.phases.typing

import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Predicate
import cats.implicits._
import evolution.compiler.phases.typing.model.Substitution
import pprint.PPrinter.Color
import evolution.compiler.phases.typing.model.Assignment
import pprint.Tree

object UnifyPredicates {

  val logger = NoOpLogger
  //val logger = ColorPPrinterLogger

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

  def unifyOld(instances: List[Predicate], predicates: List[Predicate]): Either[String, Substitution] = {
    log("Unique Predicates:")
    log(predicates.distinct)

    val predicateToSubstitutions: Map[Predicate, List[Substitution]] =
      logTime("Compute Substitutions") {
        predicates.distinct.map { predicate =>
          predicate ->
            instances.flatMap(instance => matchPredicateWithInstance(instance, predicate))
        }.toMap
      }

    val reducedPredicateToSubstitutions = logTime("Remove Empty Subst") {
      predicateToSubstitutions.filterNot {
        case (_, substitutions) => hasEmptySubstitution(substitutions)
      }
    }

    val orderedSubstitutions = logTime("Order substs") {
      reducedPredicateToSubstitutions.toList.sortBy {
        case (pred, _) => freeVarsInPredicate(pred)
      }
    }

    val optimisedSubstitutions = logTime("Reduce Substitutions")(reduceSubstitutions(orderedSubstitutions))

    logTime("Log predicates") {
      log("Ordered substitutions:")
      log(orderedSubstitutions)
      log("Optimised substitutions:")
      log(optimisedSubstitutions)
    }

    val combinations = logTime("build product") { product(optimisedSubstitutions.map(_._2)) }

    logTime("Time to unify") {
      combinations
        .flatMap(mergeSubstitutions)
        .headOption
        .toRight(s"Not able to unify predicates:\n${predicates.distinct.mkString("\n")}")
    }
  }

  private def reduceSubstitutions(
    substitutions: List[(Predicate, List[Substitution])]
  ): List[(Predicate, List[Substitution])] =
    substitutions.foldLeft(substitutions) {
      case (optimisedSoFar, (_, ss)) =>
        optimisedSoFar.map { case (p2, ss2) => p2 -> removeImpossibleSubstitutions(ss)(ss2) }
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

  private def logTime[T](message: String)(t: => T): T = {
    val start = System.currentTimeMillis()
    val result = t
    val stop = System.currentTimeMillis()
    log(s"Time to $message: ${stop - start}ms")
    result
  }

  case class Alternatives(variable: String, alternatives: Set[Type]) {
    def isFinal: Boolean = alternatives.size == 1
    def assignment: Option[Assignment] = alternatives.headOption.map(Assignment(variable, _))
    def isCompatibleWithSubstitution(subst: Substitution): Boolean =
      subst.assignments.forall(isCompatibleWithAssignment)
    def isCompatibleWithAssignment(assignment: Assignment): Boolean =
      assignment.variable != variable || alternatives.contains(assignment.tpe)
  }
  case class PredicateConditions(predicate: Predicate, substitutions: Set[Substitution]) {
    lazy val requirements: Map[String, Alternatives] =
      substitutions.flatMap(_.assignments).groupBy(_.variable).map {
        case (variable, assignments) => variable -> Alternatives(variable, assignments.map(_.tpe))
      }
    lazy val isFinal: Boolean = requirements.values.forall(_.isFinal)
    def nonEmpty: Boolean = substitutions.nonEmpty
    def assignments: List[Assignment] = requirements.values.flatMap(_.assignment).toList
    def substitution: Either[String, Substitution] =
      Either.cond(nonEmpty, Substitution(assignments), s"Predicate $predicate has no possible substitutions")

    def reduce(otherRequirements: Map[String, Alternatives]): Either[String, PredicateConditions] =
      PredicateConditions(
        predicate,
        substitutions.filter(subst => otherRequirements.values.forall(_.isCompatibleWithSubstitution(subst)))
      ).asRight.filterOrElse(_.nonEmpty, s"Predicate $predicate is incompatible with requirements $otherRequirements")

  }

  final case class PredicatesSolver(current: PredicateConditions, otherConditions: Vector[PredicateConditions]) {
    def total = 1 + otherConditions.size
    def allConditions: Seq[PredicateConditions] = current +: otherConditions
    lazy val isFinal: Boolean = current.isFinal && otherConditions.forall(_.isFinal)

    def substitution: Either[String, Substitution] =
      allConditions.toList.traverse(_.substitution).map { ss =>
        ss.fold(Substitution.empty)((s1, s2) => s1.compose(s2))
      }

    def next: Either[String, PredicatesSolver] = {
      val requirements = current.requirements
      for {
        otherConditionsReduced <- otherConditions.traverse(_.reduce(requirements))
        conditions = otherConditionsReduced :+ current
        nextSolver = PredicatesSolver(conditions.head, conditions.tail)
      } yield nextSolver
    }

    def nextN(n: Int): Either[String, PredicatesSolver] =
      if (n <= 0) this.asRight
      else next.flatMap(_.nextN(n - 1))

    def cycle: Either[String, PredicatesSolver] = nextN(total)

    //@tailrec
    def solve: Either[String, Substitution] = {
      val reducedN = cycle
      log("----------New Cycle--------------")
      log(this)
      //log(reducedN)

      reducedN match {
        case Left(value)                             => Left(value)
        case Right(nextSolver) if nextSolver == this => this.substitution
        case Right(nextSolver)                       => nextSolver.solve
      }
    }
  }
}

trait Logger {
  def log(any: Any): Unit
}

object ColorPPrinterLogger extends Logger {
  val pprinter = Color.copy(additionalHandlers = {
    case Assignment(from, to) => Tree.Infix(Color.treeify(from), "->", Color.treeify(to))
  })

  def log(any: Any): Unit = pprinter.pprintln(any, height = Int.MaxValue)
}

object NoOpLogger extends Logger {
  def log(any: Any): Unit = ()
}
