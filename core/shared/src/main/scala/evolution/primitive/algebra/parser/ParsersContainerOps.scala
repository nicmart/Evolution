package evolution.primitive.algebra.parser

import fastparse.noApi.Parser
import ParsersContainerOps._

@deprecated
class ParsersContainerOps[C](container: C) {
  def dependentParser[F[_], T](implicit hasValue: HasParser[C, F, T]): DependentParser[C, F[T]] =
    hasValue.get(container)
  def withDependentParser[F[_], T](
    dependentParser: DependentParser[C, F[T]]
  )(implicit hasValue: HasParser[C, F, T]): C =
    hasValue.set(container, dependentParser)
  def addParser[F[_], T](dependentParser: DependentParser[C, F[T]])(implicit hasValue: HasParser[C, F, T]): C =
    hasValue.set(container, container.dependentParser[F, T].or(dependentParser))
  def parser[F[_], T](implicit hasValue: HasParser[C, F, T]): Parser[F[T]] =
    hasValue.get(container).parser(container)
  def vars(implicit hasVars: HasVariables[C]): List[String] = hasVars.vars(container)
  def addVar(name: String)(implicit hasVars: HasVariables[C]): C = hasVars.addVar(name, container)
}

object ParsersContainerOps {
  implicit def ops[C](container: C): ParsersContainerOps[C] = new ParsersContainerOps[C](container)
}
