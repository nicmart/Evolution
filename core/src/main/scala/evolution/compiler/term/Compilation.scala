package evolution.compiler.term

import evolution.compiler.types.TypeClasses.Predicate

private[term] case class Compilation[+T](run: CompilerState => Either[String, (CompilerState, T)])

private[term] object Compilation {
  def error(message: String): Compilation[Nothing] =
    Compilation(_ => Left(message))
  def predName(predicate: Predicate): Compilation[String] =
    Compilation(state => state.predName(predicate).toRight(s"Pred not found: $predicate").map((state, _)))
  def withPred(predicate: Predicate): Compilation[Unit] =
    Compilation(state => Right((state.withPred(predicate), ())))
}
