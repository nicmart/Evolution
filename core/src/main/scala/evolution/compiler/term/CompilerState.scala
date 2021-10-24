package evolution.compiler.term

import evolution.compiler.types.TypeClasses.Predicate

private[term] case class CompilerState(count: Int, predicateNames: Map[Predicate, String]):
  private def currentPredVarname: String = s"$$PRED$$P$count"
  private def withNewPredVar: CompilerState = copy(count = count + 1)
  def predName(predicate: Predicate): Option[String] = predicateNames.get(predicate)
  def withPredicate(predicate: Predicate): CompilerState =
    if predicateNames.isDefinedAt(predicate) then this
    else
      copy(predicateNames = predicateNames.updated(predicate, s"${predicate.id}$$$currentPredVarname")).withNewPredVar
  def withPredicates(predicates: List[Predicate]): CompilerState =
    predicates.foldLeft(this) { (compilerState, predicate) =>
      compilerState.withPredicate(predicate)
    }

private[term] object CompilerState:
  val empty: CompilerState = CompilerState(0, Map.empty)
