package evolution.compiler.phases.typer

import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Predicate
import cats.implicits._
import evolution.compiler.phases.typer.model.Substitution
import evolution.compiler.phases.typer.model.Assignment
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

  private def matchPredicateWithInstance(instance: Predicate, predicate: Predicate): Option[InstanceSubstitution] =
    (instance, predicate) match {
      case (Predicate(iId, iTypes), Predicate(pId, pTypes)) if iId == pId =>
        matchTypes(iTypes, pTypes).map(substitution => InstanceSubstitution(instance, substitution))
      case _ => None
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

  sealed abstract case class TypeInInstance(instance: Predicate, tpe: Type, position: Int)

  object TypeInInstance {
    def apply(instance: Predicate, position: Int): Option[TypeInInstance] =
      instance.types.get(position).map(tpe => new TypeInInstance(instance, tpe, position) {})
  }

  final case class Alternatives(variable: String, alternatives: Set[Type]) {
    def isFinal: Boolean = alternatives.size == 1
    // TODO: taking the head where there are multiple alternatives is wrong
    def assignment: Option[Assignment] = alternatives.headOption.map(Assignment(variable, _))
    def isCompatibleWithSubstitution(subst: Substitution): Boolean =
      subst.assignments.forall(isCompatibleWithAssignment)
    def isCompatibleWithAssignment(assignment: Assignment): Boolean =
      assignment.variable != variable || alternatives.contains(assignment.tpe)
  }

  case class InstanceSubstitution(instance: Predicate, substitution: Substitution)

  case class PredicateConditions(predicate: Predicate, instanceSubstitutions: Set[InstanceSubstitution]) {
    lazy val requirements: Map[String, Alternatives] =
      instanceSubstitutions.flatMap(_.substitution.assignments).groupBy(_.variable).map {
        case (variable, assignments) => variable -> Alternatives(variable, assignments.map(_.tpe))
      }
    lazy val isFinal: Boolean = requirements.values.forall(_.isFinal)
    def nonEmpty: Boolean = instanceSubstitutions.nonEmpty
    def assignments: List[Assignment] = requirements.values.flatMap(_.assignment).toList
    def substitution: Either[String, Substitution] =
      Either.cond(nonEmpty, Substitution(assignments), s"Predicate $predicate has no possible substitutions")

    def reduce(otherRequirements: Map[String, Alternatives]): Either[String, PredicateConditions] =
      PredicateConditions(
        predicate,
        instanceSubstitutions.filter(
          instSubst => otherRequirements.values.forall(_.isCompatibleWithSubstitution(instSubst.substitution))
        )
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

    def solve: Either[String, Substitution] = {
      val reducedN = cycle
      reducedN match {
        case Left(value)                             => Left(value)
        case Right(nextSolver) if nextSolver == this => this.substitution
        case Right(nextSolver)                       => nextSolver.solve
      }
    }
  }
}
