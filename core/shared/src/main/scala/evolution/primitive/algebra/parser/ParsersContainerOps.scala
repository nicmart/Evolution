package evolution.primitive.algebra.parser

import evolution.data.HasValue
import fastparse.noApi.Parser
import ParsersContainerOps._
import evolution.primitive.algebra.parser.DependentParser.HasParser

class ParsersContainerOps[C](container: C) {
  def dependentParser[T](implicit hasValue: HasParser[C, T]): DependentParser[C, T] =
    hasValue.get(container)
  def withDependentParser[T](dependentParser: DependentParser[C, T])(implicit hasValue: HasParser[C, T]): C =
    hasValue.set(container, dependentParser)
  def addParser[T](dependentParser: DependentParser[C, T])(implicit hasValue: HasParser[C, T]): C =
    hasValue.set(container, container.dependentParser[T].or(dependentParser))
  def parser[T](implicit hasValue: HasValue[C, DependentParser[C, T]]): Parser[T] =
    hasValue.get(container).parser(container)
  def vars(implicit hasVars: HasVariables[C]): List[String] = hasVars.vars(container)
  def addVar(name: String)(implicit hasVars: HasVariables[C]): C = hasVars.addVar(name, container)
}

object ParsersContainerOps {
  implicit def ops[C](container: C): ParsersContainerOps[C] = new ParsersContainerOps[C](container)
}
