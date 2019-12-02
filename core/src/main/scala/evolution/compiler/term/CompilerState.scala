package evolution.compiler.term

import evolution.compiler.types.TypeClasses.Predicate

private[term] case class CompilerState(count: Int, predicateNames: Map[Predicate, String]) {
  private def currentPredVarname: String = s"P$count"
  private def withNewPredVar: CompilerState = copy(count = count + 1)
  def predName(predicate: Predicate): Option[String] = predicateNames.get(predicate)
  def withPred(predicate: Predicate): CompilerState =
    if (predicateNames.isDefinedAt(predicate)) this
    else copy(predicateNames = predicateNames.updated(predicate, currentPredVarname)).withNewPredVar
}
