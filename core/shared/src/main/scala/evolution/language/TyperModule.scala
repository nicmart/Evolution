package evolution.language

import cats.data.State
import cats.{ Monad, MonadError }
import cats.implicits._

trait TyperModule[F[_]] { self: ASTModule[F] =>
  import AST._, TypeClasses._

  object Typer {

    type TypeInference[A] = State[TypeInference.State, A]

    object TypeInference {
      case class State(vars: TypeVars, subst: Substitution)
      val empty = State(TypeVars.empty, Substitution.empty)
      implicit class TypeInferenceOps[T](ti: TypeInference[T]) {
        def evaluate: T = ti.runA(TypeInference.empty).value
      }
    }

    def newVar: TypeInference[Type.Var] = State { s =>
      (s.copy(vars = s.vars.next), s.vars.current)
    }

    /**
     * TODO: can we express this with transformChildren or similar?
     * TODO: can we assign vars directly when we unify?
     * Traverse the AST and assign type variables to each expression.
     * No constraint is added at this stage
     */
    def assignVars(expr: AST): TypeInference[AST] =
      expr match {
        case _ if expr.tpe != Type.Var("") =>
          State.pure(expr)

        case AST.App(f, in, _) =>
          (assignVars(f), assignVars(in), newVar).mapN { (transformedF, transformedIn, t) =>
            App(transformedF, transformedIn, t)
          }

        case Lambda(varName, lambdaBody, _) =>
          (assignVars(varName), assignVars(lambdaBody), newVar).mapN { (v, b, t) =>
            Lambda(varName.copy(tpe = v.tpe), b, t)
          }

        case Let(varName, value, in, _) =>
          (assignVars(varName), assignVars(value), assignVars(in), newVar).mapN { (tVar, tValue, tIn, tLet) =>
            Let(varName.copy(tpe = tVar.tpe), tValue, tIn, tLet)
          }

        case Const(id, _, _) =>
          freshInstanceSubstitution(id.scheme).map(subst =>
            Const(id, subst.substitute(id.scheme), subst.substitute(id.predicates)))

        case _ => // No-children expressions. Unsafe, that's why I would like to use transformChildren method
          newVar.map(expr.withType)
      }

    def findConstraints(expr: AST): TypeInference[Constraints] = {
      val nodeConstraints: TypeInference[Constraints] = expr match {
        case Var(_, _)       => Constraints.empty.pure[TypeInference]
        case Const(_, _, ps) => Constraints.empty.withPredicates(ps).pure[TypeInference]
        case Number(_, tpe)  => Constraints.empty.withPredicate(Predicate("Num", List(tpe))).pure[TypeInference]
        case App(Const(Constant.Lift, _, _), value, tpe) =>
          Constraints(tpe -> lift(value.tpe)).pure[TypeInference]
        case App(f, x, tpe) => Constraints(f.tpe -> (x.tpe =>: tpe)).pure[TypeInference]
        case Lambda(variable, lambdaExpr, tpe) =>
          val arrowConstraint = Constraints(tpe -> Type.Arrow(variable.tpe, lambdaExpr.tpe))
          val variableConstraints =
            Constraints(varUsagesIn(variable.name, lambdaExpr).map(u => u.tpe -> variable.tpe): _*)
          arrowConstraint.merge(variableConstraints).pure[TypeInference]
        case Let(variable, value, in, tpe) =>
          Constraints(varUsagesIn(variable.name, in).map(u => u.tpe -> variable.tpe): _*)
            .merge(Constraints(variable.tpe -> value.tpe, in.tpe -> tpe))
            .pure[TypeInference]
        case _ => ???
      }

      val childrenConstraints = expr.children.traverse(findConstraints)
      (nodeConstraints, childrenConstraints).mapN { (n, c) =>
        n.merge(c)
      }
    }

    def assignVarsAndFindConstraints(expr: AST): TypeInference[(AST, Constraints)] =
      for {
        exprWithVars <- assignVars(expr)
        constraints <- findConstraints(exprWithVars)
      } yield (exprWithVars, constraints)

    case class Unification(substitution: Substitution, predicates: List[Predicate]) {
      def compose(s2: Substitution): Unification = copy(substitution = substitution.compose(s2))
      def withPredicate(predicate: Predicate): Unification = copy(predicates = predicate :: predicates)
      def substitutedPredicates: List[Predicate] = substitution.substitute(predicates)
    }

    object Unification {
      val empty = Unification(Substitution.empty, Nil)
    }

    def unify[M[_]](constraints: Constraints)(implicit M: MonadError[M, String]): M[Unification] =
      constraints.constraints match {
        case Nil => Unification.empty.pure[M]
        case head :: tail =>
          head match {
            case Constraint.Eq(a, b) if a == b => unify[M](Constraints(tail))
            case Constraint.Eq(Type.Var(x), t) if typeVarUsagesIn(x, t).isEmpty =>
              val substituteVar = Substitution(x -> t)
              val constraints2 = substituteVar.substitute(Constraints(tail))
              unify[M](constraints2).map(_.compose(substituteVar))
            case Constraint.Eq(t, Type.Var(x)) if typeVarUsagesIn(x, t).isEmpty =>
              val substituteVar = Substitution(x -> t)
              val constraints2 = substituteVar.substitute(Constraints(tail))
              unify[M](constraints2).map(_.compose(substituteVar))
            case Constraint.Eq(Type.Evo(a), Type.Evo(b)) =>
              unify[M](Constraints(a -> b).merge(Constraints(tail)))
            case Constraint.Eq(Type.Arrow(a1, b1), Type.Arrow(a2, b2)) =>
              unify[M](Constraints(a1 -> a2, b1 -> b2).merge(Constraints(tail)))
            case Constraint.Pred(p) => unify[M](Constraints(tail)).map(_.withPredicate(p))
            case _                  => s"$head constraint can't be unified".raiseError[M, Unification]
          }
      }

    // TODO: Very, Very naive typeclass checking, that works for now because we just have typeclasses without
    // without derivation
    def checkPredicates[M[_]](predicates: List[Predicate])(implicit M: MonadError[M, String]): M[Unit] = {
      val predicatesWithoutVars = predicates.filter(p => p.types.flatMap(typeVars).isEmpty)
      val invalidPredicates = predicatesWithoutVars.filter(p => !instances.contains(p))
      if (invalidPredicates.isEmpty) M.pure(())
      else M.raiseError(s"Not found instances for predicates $invalidPredicates")
    }

    private def varUsagesIn(varName: String, expr: AST): List[AST] =
      expr match {
        case Var(name, _) if name == varName                         => List(expr)
        case Lambda(Var(lambdaVar, _), _, _) if lambdaVar == varName => Nil // Shadowing
        case Let(Var(letVar, _), value, in, _) if letVar == varName  => varUsagesIn(varName, value) // Shadowing
        case _                                                       => expr.children.flatMap(varUsagesIn(varName, _))
      }

    private def typeVarUsagesIn(varName: String, tpe: Type): List[Type] =
      tpe match {
        case Type.Var(name) if name == varName => List(tpe)
        case Type.Evo(inner)                   => typeVarUsagesIn(varName, inner)
        case Type.Lst(inner)                   => typeVarUsagesIn(varName, inner)
        case Type.Arrow(from, to)              => typeVarUsagesIn(varName, from) ++ typeVarUsagesIn(varName, to)
        case _                                 => Nil
      }

    private def typeVars(tpe: Type): Set[Type.Var] = tpe match {
      case Type.Var(name) => Set(Type.Var(name))
      case _              => tpe.children.flatMap(typeVars).toSet
    }

    private def freshInstanceSubstitution(scheme: Type): TypeInference[Substitution] = {
      val varsInShceme = typeVars(scheme).toList
      for {
        assignments <- varsInShceme.traverse(schemeVar => newVar.map(typeVar => Assignment(schemeVar.name, typeVar)))
        substitution = Substitution(assignments)
      } yield substitution
    }

    private def freshInstance(scheme: Type): TypeInference[Type] =
      freshInstanceSubstitution(scheme).map(_.substitute(scheme))

    sealed trait Constraint
    object Constraint {
      case class Eq(a: Type, b: Type) extends Constraint {
        override def toString = s"$a = $b"
      }

      case class Pred(predicate: Predicate) extends Constraint {
        override def toString = s"${predicate.id} ${predicate.types.mkString(" ")}"
      }
    }

    case class Constraints(constraints: List[Constraint]) {
      def merge(other: Constraints): Constraints = Constraints(constraints ++ other.constraints)
      def merge(other: List[Constraints]): Constraints = other.foldLeft(this) { (constraints, current) =>
        constraints.merge(current)
      }
      def withPredicate(predicate: Predicate): Constraints =
        Constraints(Constraint.Pred(predicate) :: constraints)

      def withPredicates(predicates: List[Predicate]): Constraints =
        Constraints(predicates.map(Constraint.Pred) ++ constraints)

      override def toString: String = constraints.mkString("\n")
    }

    object Constraints {
      val empty: Constraints = Constraints(Nil)
      def apply(constraints: (Type, Type)*): Constraints = Constraints(constraints.toList.map {
        case (a, b) => Constraint.Eq(a, b)
      })
    }

    case class Assignment(variable: String, tpe: Type)

    case class Substitution(assignments: List[Assignment]) {
      def lookup(variable: String): Option[Type] = assignments.find(_.variable == variable).map(_.tpe)
      def substitute[T](t: T)(implicit cbs: CanBeSubstituted[T]): T = cbs.substitute(this, t)
      def compose(s2: Substitution): Substitution = Substitution(substitute(s2).assignments ++ assignments)
      def merge[M[_]](s2: Substitution)(implicit M: MonadError[M, String]): M[Substitution] = {
        val commonVars = assignments.map(_.variable).intersect(s2.assignments.map(_.variable))
        val agree = commonVars.forall { variable =>
          substitute[Type](Type.Var(variable)) == s2.substitute[Type](Type.Var(variable))
        }
        if (agree) M.pure(Substitution(assignments ++ s2.assignments))
        else M.raiseError("Merge has failed")
      }
    }

    object Substitution {
      def apply(assignments: (String, Type)*): Substitution = Substitution(assignments.toList.map {
        case (s, t) => Assignment(s, t)
      })
      val empty: Substitution = Substitution(Nil)
    }

    trait CanBeSubstituted[T] {
      def substitute(s: Substitution, t: T): T
    }

    object CanBeSubstituted {
      implicit val `type`: CanBeSubstituted[Type] = new CanBeSubstituted[Type] {
        def substitute(s: Substitution, t: Type): Type = t match {
          case Type.Var(name)       => s.lookup(name).getOrElse(t)
          case Type.Evo(inner)      => Type.Evo(substitute(s, inner))
          case Type.Lst(inner)      => Type.Lst(substitute(s, inner))
          case Type.Arrow(from, to) => Type.Arrow(substitute(s, from), substitute(s, to))
          case _                    => t
        }
      }

      implicit val assignment: CanBeSubstituted[Assignment] = new CanBeSubstituted[Assignment] {
        def substitute(s: Substitution, a: Assignment): Assignment = a.copy(tpe = s.substitute(a.tpe))
      }

      implicit val subst: CanBeSubstituted[Substitution] = new CanBeSubstituted[Substitution] {
        def substitute(s1: Substitution, s2: Substitution): Substitution = Substitution(s1.substitute(s2.assignments))
      }

      implicit val canBeSubstituted: CanBeSubstituted[Predicate] = new CanBeSubstituted[Predicate] {
        def substitute(s: Substitution, predicate: Predicate): Predicate =
          predicate.copy(types = s.substitute(predicate.types))
      }

      implicit def list[T](implicit inner: CanBeSubstituted[T]): CanBeSubstituted[List[T]] =
        new CanBeSubstituted[List[T]] {
          def substitute(s1: Substitution, ts: List[T]): List[T] = ts.map(s1.substitute[T])
        }

      implicit val ast: CanBeSubstituted[AST] = new CanBeSubstituted[AST] {
        def substitute(s: Substitution, ast: AST): AST =
          AST.transformRecursively(ast, tree => tree.withType(s.substitute(tree.tpe)))
      }

      implicit val constraint: CanBeSubstituted[Constraint] = new CanBeSubstituted[Constraint] {
        def substitute(s: Substitution, constraint: Constraint): Constraint =
          constraint match {
            case Constraint.Eq(a, b) => Constraint.Eq(s.substitute(a), s.substitute(b))
            case Constraint.Pred(p)  => Constraint.Pred(s.substitute(p))
          }
      }

      implicit val constraints: CanBeSubstituted[Constraints] = new CanBeSubstituted[Constraints] {
        def substitute(s: Substitution, constraints: Constraints): Constraints =
          Constraints(s.substitute(constraints.constraints))
      }
    }

    class TypeVars(total: Int) {
      def current: Type.Var = Type.Var(s"T$total")
      def next: TypeVars = new TypeVars(total + 1)
    }

    object TypeVars {
      val empty = new TypeVars(0)
    }

    val instances: List[Predicate] = List(
      Predicate("Num", List(Type.Dbl)),
      Predicate("Num", List(Type.Integer)),
      Predicate("Semigroup", List(Type.Integer)),
      Predicate("Semigroup", List(Type.Dbl)),
      Predicate("Semigroup", List(Type.Point))
    )
  }
}