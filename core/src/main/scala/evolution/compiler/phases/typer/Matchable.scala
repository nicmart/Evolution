package evolution.compiler.phases.typer

import evolution.compiler.phases.typer.model.Substitution
import evolution.compiler.tree.TypedTree
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}

// A subst that transform the SECOND arg to the FIRST
trait Matchable[T]:
  def substitution(t1: T, t2: T): Option[Substitution]
  extension (t1: T) final def substitute(t2: T): Option[Substitution] = substitution(t1, t2)

object Matchable:
  def tryMatch[T: Matchable](t1: T, t2: T): Option[Substitution] = t1.substitute(t2)

  given Matchable[Type] =
    case (t1, Type.Var(name))         => Some(Substitution(name -> t1))
    case (Type.Evo(t1), Type.Evo(t2)) => t1.substitute(t2)
    case (Type.Lst(t1), Type.Lst(t2)) => t1.substitute(t2)
    case (Type.Arrow(t11, t12), Type.Arrow(t21, t22)) =>
      for
        s1 <- t11.substitute(t21)
        s2 <- t12.substitute(t22)
        s <- s1.mergeOpt(s2)
      yield s
    case (t1, t2) if t1 == t2 => Some(Substitution.empty)
    case _                    => None

  given [T: Matchable]: Matchable[List[T]] =
    case (iHead :: iTail, pHead :: pTail) =>
      for
        tailSubst <- iTail.substitute(pTail)
        headSubst <- tryMatch(iHead, pHead)
        subst <- headSubst.merge(tailSubst).toOption
      yield subst

    case (Nil, Nil) => Some(Substitution.empty)
    case _          => None

  given Matchable[Predicate] =
    case (p1, p2) if p1.id == p2.id =>
      tryMatch(p1.types, p2.types)
    case _ => None

  given Matchable[TypedTree] =
    case (tt1, tt2) =>
      tryMatch(tt1.annotation, tt2.annotation)

  given [T: Matchable]: Matchable[Qualified[T]] =
    (q1, q2) =>
      for
        predSubst <- tryMatch(q1.predicates, q2.predicates)
        tSubst <- tryMatch(q1.value, q2.value)
        subst <- predSubst.merge(tSubst).toOption
      yield subst
