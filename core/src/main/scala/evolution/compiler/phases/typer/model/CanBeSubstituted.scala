package evolution.compiler.phases.typer.model

import evolution.compiler.types.Type
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}
import evolution.compiler.tree._

trait CanBeSubstituted[T]:
  def substitute(s: Substitution, t: T): T

object CanBeSubstituted:
  given CanBeSubstituted[Type] with
    def substitute(s: Substitution, t: Type): Type =
      Type.replaceVars(t, s.assignments.map(a => a.variable -> a.tpe))

  given CanBeSubstituted[Assignment] with
    def substitute(s: Substitution, a: Assignment): Assignment = a.copy(tpe = s.substitute[Type](a.tpe))

  given CanBeSubstituted[Substitution] with
    def substitute(s1: Substitution, s2: Substitution): Substitution = Substitution(s1.substitute(s2.assignments))

  given CanBeSubstituted[Predicate] with
    def substitute(s: Substitution, predicate: Predicate): Predicate =
      predicate.copy(types = s.substitute(predicate.types))

  given CanBeSubstituted[TypedTree] with
    def substitute(s: Substitution, typedTree: TypedTree): TypedTree =
      AnnotatedTree.catamorphism[TypedTree, Qualified[Type]]((qt, treeF) => treeF.annotate(s.substitute(qt)))(typedTree)

  given [T](using CanBeSubstituted[T]): CanBeSubstituted[List[T]] with
    def substitute(s1: Substitution, ts: List[T]): List[T] =
      ts.map(s1.substitute[T])

  given [T](using CanBeSubstituted[T]): CanBeSubstituted[Qualified[T]] with
    def substitute(s: Substitution, qt: Qualified[T]): Qualified[T] =
      Qualified(s.substitute(qt.predicates), s.substitute(qt.value))
