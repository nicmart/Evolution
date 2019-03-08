package evolution.language

import cats.{ Monad, MonadError }
import cats.data.{ ReaderT, State, StateT }
import cats.implicits._

trait TyperModule[F[_]] { self: ASTModule[F] =>
  import AST._
  import TypeClasses._

  object Typer {

    type TypeInferenceState[T] = StateT[Either[String, ?], TypeInference.State, T]
    val TIS = Monad[TypeInferenceState]

    type BindingContext = Map[String, TypeInferenceState[Qualified[Type]]]

    implicit class BindingContextOps(ctx: BindingContext) {
      def getBinding(name: String): TypeInferenceState[Qualified[Type]] =
        ctx.get(name) match {
          case None =>
            StateT(s =>
              Left[String, (TypeInference.State, Qualified[Type])](s"Unable to find type binding for variable $name"))
          case Some(tis) => tis
        }
    }

    type TypeInference[T] = ReaderT[TypeInferenceState, BindingContext, T]
    val TI = MonadError[TypeInference, String]

    object TypeInference {
      case class State(vars: TypeVars, subst: Substitution)
      val empty = State(TypeVars.empty, Substitution.empty)

      def stateless[T](f: BindingContext => Either[String, T]): TypeInference[T] = ReaderT(
        ctx => StateT(s => f(ctx).map(t => (s, t))))

      def newVarS: TypeInferenceState[Type.Var] =
        StateT { s =>
          Right((s.copy(vars = s.vars.next), s.vars.current))
        }

      def newVar: TypeInference[Type.Var] = ReaderT(_ => newVarS)

      //def pushVar(name: String, tpe: Type): TypeInference[]
      def getBinding(name: String): TypeInference[Qualified[Type]] = ReaderT(_.getBinding(name))

      def pushFreshBinding[T](name: String)(ti: TypeInference[T]): TypeInference[T] =
        for {
          tpe <- newVar
          t <- ti.local[BindingContext](_.updated(name, StateT.pure(Qualified(tpe))))
        } yield t

      implicit class TypeInferenceOps[T](ti: TypeInference[T]) {
        def evaluate: T = ti.run(Map.empty).runA(TypeInference.empty).value.right.get
        def evaluateWith(ctx: BindingContext): T = ti.run(ctx).runA(TypeInference.empty).value.right.get
      }
    }

    import TypeInference._

    /**
     * TODO: can we express this with transformChildren or similar?
     * TODO: can we assign vars directly when we unify?
     * Traverse the AST and assign type variables to each expression.
     * No constraint is added at this stage
     */
    def assignVars(expr: AST): TypeInference[AST] =
      expr match {
        case _ if expr.tpe != Type.Var("") =>
          TI.pure(expr)

        case AST.App(f, in, _) =>
          (assignVars(f), assignVars(in), newVar).mapN { (transformedF, transformedIn, t) =>
            App(transformedF, transformedIn, t)
          }

        case Lambda(varName, lambdaBody, _) =>
          (pushFreshBinding(varName)(assignVars(lambdaBody)), newVar).mapN { (b, t) =>
            Lambda(varName, b, t)
          }

        case Let(varName, value, in, _) =>
          (assignVars(value), pushFreshBinding(varName)(assignVars(in)), newVar).mapN { (tValue, tIn, tLet) =>
            Let(varName, tValue, tIn, tLet)
          }

        case Const(id, _, _) =>
          freshInstanceSubstitution(id.scheme).map(subst =>
            Const(id, subst.substitute(id.scheme), subst.substitute(id.predicates)))

        case Var(name, _) =>
          getBinding(name).map(qt => Var(name, qt.t))

        case _ => // No-children expressions. Unsafe, that's why I would like to use transformChildren method
          newVar.map(expr.withType)
      }

//    val constantQualifiedTypes: BindingContext = {
//        Constant.values.map { constant =>
//          constant.entryName -> ???
//
//        }
//    }

    def findConstraints(expr: AST): TypeInference[Constraints] = {
      val nodeConstraints: TypeInference[Constraints] = expr match {
        case Var(_, _)       => Constraints.empty.pure[TypeInference]
        case Const(_, _, ps) => Constraints.empty.withPredicates(ps).pure[TypeInference]
        case Number(_, tpe)  => Constraints.empty.withPredicate(Predicate("Num", List(tpe))).pure[TypeInference]
        case Bool(_, tpe)    => Constraints(tpe -> Type.Bool).pure[TypeInference]
        case App(Const(Constant.Lift, _, _), value, tpe) =>
          Constraints(tpe -> lift(value.tpe)).pure[TypeInference]
        case App(f, x, tpe) => Constraints(f.tpe -> (x.tpe =>: tpe)).pure[TypeInference]
        case Lambda(variable, lambdaExpr, tpe) =>
          for {
            variableType <- newVar
            arrowConstraint = Constraints(tpe -> Type.Arrow(variableType, lambdaExpr.tpe))
            variableConstraints = Constraints(varUsagesIn(variable, lambdaExpr).map(u => u.tpe -> variableType): _*)
          } yield arrowConstraint.merge(variableConstraints)
        case Let(variable, value, in, tpe) =>
          for {
            variableType <- newVar
            mainConstraint = Constraints(tpe -> in.tpe, variableType -> value.tpe)
            variableConstraints = Constraints(varUsagesIn(variable, in).map(u => u.tpe -> variableType): _*)
          } yield mainConstraint.merge(variableConstraints)
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

    // TODO: Very, Very naive typeclass checking, that works for now because we just have typeclasses without derivation
    def checkPredicates[M[_]](predicates: List[Predicate])(implicit M: MonadError[M, String]): M[Unit] = {
      val predicatesWithoutVars = predicates.filter(p => p.types.flatMap(typeVars).isEmpty)
      val invalidPredicates = predicatesWithoutVars.filter(p => !instances.contains(p))
      if (invalidPredicates.isEmpty) M.pure(())
      else M.raiseError(s"Not found instances for predicates $invalidPredicates")
    }

    private def varUsagesIn(varName: String, expr: AST): List[AST] =
      expr match {
        case Var(name, _) if name == varName                 => List(expr)
        case Lambda(lambdaVar, _, _) if lambdaVar == varName => Nil // Shadowing
        case Let(letVar, value, in, _) if letVar == varName  => varUsagesIn(varName, value) // Shadowing
        case _                                               => expr.children.flatMap(varUsagesIn(varName, _))
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

    private def freshQualifiedType(qt: Qualified[Type]): TypeInferenceState[Qualified[Type]] = {
      val varsInShceme = typeVars(qt.t).toList
      for {
        assignments <- varsInShceme.traverse(schemeVar => newVarS.map(typeVar => Assignment(schemeVar.name, typeVar)))
        substitution = Substitution(assignments)
      } yield substitution.substitute(qt)
    }

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

    private trait CanBeSubstituted[T] {
      def substitute(s: Substitution, t: T): T
    }

    private object CanBeSubstituted {
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

      implicit def qualified[T](implicit inner: CanBeSubstituted[T]): CanBeSubstituted[Qualified[T]] =
        new CanBeSubstituted[Qualified[T]] {
          def substitute(s: Substitution, qt: Qualified[T]): Qualified[T] =
            Qualified(s.substitute(qt.predicates), s.substitute(qt.t))
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
