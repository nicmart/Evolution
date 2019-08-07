package evolution.language

import cats.Applicative
import cats.implicits._
import cats.mtl.FunctorRaise
import cats.mtl.implicits._
import evolution.compiler.ast.AST
import evolution.compiler.ast.AST._
import evolution.compiler.phases.typing.{ Constraint, Constraints, Substitution, TypeInference }
import evolution.compiler.phases.typing.TypeInference.TypeInferenceInstances
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses._
import evolution.compiler.phases.typing.TypingConfig

object Typer {

  /**
   * TODO: can we express this with transformChildren or similar?
   * TODO: can we assign vars directly when we unify?
   * Traverse the AST and assign type variables to each expression.
   * No constraint is added at this stage
   */
  def assignVars[M[_]](expr: AST)(implicit TI: TypeInference[M]): M[AST] = {
    import TypeInference._
    expr match {
      case _ if expr.tpe.t != Type.Var("") =>
        expr.pure[M]

      case AST.App(f, in, _) =>
        (assignVars(f), assignVars(in), newTypeVar).mapN { (transformedF, transformedIn, t) =>
          App(transformedF, transformedIn, t)
        }

      // TODO here and in Let we compute constraints. That's because later on it is not possible to find the bindings attached to varName
      case Lambda(varName, lambdaBody, _) =>
        newTypeVar.flatMap(
          qt =>
            withVarType(varName, qt) {
              assignVars(lambdaBody).map(b => Lambda(varName, b, Qualified(qt.t =>: b.tpe.t)))
            }
        )

      case Let(varName, value, body, _) =>
        assignVars(value).flatMap { valueWithVars =>
          withVarType(varName, valueWithVars.tpe) {
            assignVars(body).map(bodyWithVars => Let(varName, valueWithVars, bodyWithVars, bodyWithVars.tpe))
          }
        }

      case Identifier(name, _, _) =>
        getType(name).map[AST](ast => ast)

      case _ => // No-children expressions. Unsafe, that's why I would like to use transformChildren method
        newTypeVar.map(expr.withType)
    }
  }

  def findConstraints[M[_]](expr: AST)(implicit TI: TypeInference[M]): M[Constraints] = {
    import TypeInference._
    val nodeConstraints: M[Constraints] = expr match {
      case Identifier(_, qt, _)  => Constraints.empty.withPredicates(qt.predicates).pure[M]
      case DoubleLiteral(_, tpe) => Constraints(tpe.t -> Type.Dbl).pure[M]
      case IntLiteral(_, tpe)    => Constraints.empty.withPredicate(Predicate("Num", List(tpe.t))).pure[M]
      case Bool(_, tpe)          => Constraints(tpe.t -> Type.Bool).pure[M]
      case App(f, x, tpe)        => Constraints(f.tpe.t -> (x.tpe.t =>: tpe.t)).pure[M]
      case Lambda(_, _, _) =>
        Constraints.empty.pure[M]
      case Let(_, _, _, _) =>
        Constraints.empty.pure[M]
    }

    val childrenConstraints = expr.children.traverse(findConstraints[M])

    (nodeConstraints, childrenConstraints).mapN { (n, c) =>
      n.merge(c)
    }
  }

  def assignVarsAndFindConstraints[M[_]](expr: AST)(implicit TI: TypeInference[M]): M[(AST, Constraints)] = {
    import TypeInference._
    for {
      exprWithVars <- assignVars(expr)
      constraints <- findConstraints(exprWithVars)
    } yield (exprWithVars, constraints)
  }

  case class Unification(substitution: Substitution, predicates: List[Predicate]) {
    def compose(s2: Substitution): Unification = copy(substitution = substitution.compose(s2))
    def withPredicate(predicate: Predicate): Unification = copy(predicates = predicate :: predicates)
    def substitutedPredicates: List[Predicate] = substitution.substitute(predicates)
  }

  object Unification {
    val empty = Unification(Substitution.empty, Nil)
  }

  def unify[M[_]](constraints: Constraints)(implicit R: FunctorRaise[M, String], A: Applicative[M]): M[Unification] =
    constraints.constraints match {
      case Nil => Unification.empty.pure[M]
      case head :: tail =>
        head match {
          case Constraint.Eq(a, b) if a == b => unify[M](Constraints(tail))
          case Constraint.Eq(Type.Var(x), t) if t.typeVarUsages(x).isEmpty =>
            val substituteVar = Substitution(x -> t)
            val constraints2 = substituteVar.substitute(Constraints(tail))
            unify[M](constraints2).map(_.compose(substituteVar))
          case Constraint.Eq(t, Type.Var(x)) if t.typeVarUsages(x).isEmpty =>
            val substituteVar = Substitution(x -> t)
            val constraints2 = substituteVar.substitute(Constraints(tail))
            unify[M](constraints2).map(_.compose(substituteVar))
          case Constraint.Eq(Type.Evo(a), Type.Evo(b)) =>
            unify[M](Constraints(a -> b).merge(Constraints(tail)))
          case Constraint.Eq(Type.Arrow(a1, b1), Type.Arrow(a2, b2)) =>
            unify[M](Constraints(a1 -> a2, b1 -> b2).merge(Constraints(tail)))
          case Constraint.Pred(p) => unify[M](Constraints(tail)).map(_.withPredicate(p))
          case _                  => s"$head constraint can't be unified".raise[M, Unification]
        }
    }

  object PredicatesUnifier {
    def unify(instances: List[Predicate], predicates: List[Predicate]): Option[Substitution] = {
      val predicateToSubstitutions: Map[Predicate, List[Substitution]] =
        predicates.distinct.map { predicate =>
          predicate ->
            instances.flatMap(instance => matchPredicateWithInstance(instance, predicate))
        }.toMap

      val reducedPredicateToSubstitutions = predicateToSubstitutions.filterNot {
        case (_, substitutions) => hasEmptySubstitution(substitutions)
      }

      val orderedSubstitutions = reducedPredicateToSubstitutions.toList.sortBy {
        case (pred, _) => freeVarsInPredicate(pred)
      }.map(_._2)

      val combinations = product(orderedSubstitutions)

      combinations.flatMap(mergeSubstitutions).headOption
    }

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
            subst <- headSubst.merge[Either[String, ?]](tailSubst).toOption
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
          mergeSubstitutions(substTail).flatMap(_.merge[Either[String, ?]](substHead).toOption)
        case Nil => Some(Substitution.empty)
      }
  }

  // TODO: Very, Very naive typeclass checking, that works for now because we just have typeclasses without derivation
  def predicatesSubstitution[M[_]](predicates: List[Predicate])(implicit M: TypeInference[M]): M[Substitution] = {
    import TypeInferenceInstances._
    PredicatesUnifier.unify(TypingConfig.instances, predicates) match {
      case Some(subst) => subst.pure[M]
      case None        => s"Not able to unify predicates:\n${predicates.distinct.mkString("\n")}".raise[M, Substitution]
    }
  }

  class TypeVars(total: Int) {
    def current: Type.Var = Type.Var(s"T$total")
    def next: TypeVars = new TypeVars(total + 1)
  }

  object TypeVars {
    val empty = new TypeVars(0)
  }
}
