package evolution.language

import cats.implicits._
import cats.mtl.implicits._
import cats.mtl.{ ApplicativeAsk, ApplicativeLocal, FunctorRaise, MonadState }
import cats.{ Applicative, Monad }

trait TyperModule[F[_]] { self: ASTModule[F] with TypesModule[F] with PredefinedConstantsModule[F] =>
  import AST._
  import TypeClasses._

  object Typer {

    trait TypeInference[M[_]] {
      def E: FunctorRaise[M, String]
      def S: MonadState[M, TypeInference.State]
      def A: ApplicativeAsk[M, TypeContext]
      def L: ApplicativeLocal[M, TypeContext]
    }

    def instance[M[_]](
      implicit
      me: FunctorRaise[M, String],
      ms: MonadState[M, TypeInference.State],
      aa: ApplicativeAsk[M, TypeContext],
      al: ApplicativeLocal[M, TypeContext]
    ): TypeInference[M] =
      new TypeInference[M] {
        override def E: FunctorRaise[M, String] = me
        override def S: MonadState[M, TypeInference.State] = ms
        override def A: ApplicativeAsk[M, TypeContext] = aa
        override def L: ApplicativeLocal[M, TypeContext] = al
      }

    object TypeInference {
      case class State(vars: TypeVars, subst: Substitution)
      val empty = State(TypeVars.empty, Substitution.empty)

      def apply[M[_]](implicit ti: TypeInference[M]): TypeInference[M] = ti

      implicit def monad[M[_]](implicit ti: TypeInference[M]): Monad[M] = ti.S.monad

      def newTypeVar[M[_]](implicit TI: TypeInference[M]): M[Qualified[Type]] = for {
        state <- TI.S.get
        qt = Qualified[Type](state.vars.current)
        _ <- TI.S.set(state.copy(vars = state.vars.next))
      } yield qt

      def getType[M[_]](name: String)(implicit TI: TypeInference[M]): M[Identifier] =
        TI.A.ask.flatMap(_.getBinding(name))

      def withVarType[M[_], T](name: String, qt: Qualified[Type])(t: M[T])(implicit TI: TypeInference[M]): M[T] =
        for {
          t <- TI.L.local(_.updated(name, TypeBinding.Variable(name, qt)))(t)
        } yield t
    }

    object TypeInferenceInstances {
      implicit def monadInstance[M[_]](implicit M: TypeInference[M]): Monad[M] = M.S.monad
      implicit def functorRaise[M[_]](implicit M: TypeInference[M]): FunctorRaise[M, String] = M.E
      implicit def monadState[M[_]](implicit M: TypeInference[M]): MonadState[M, TypeInference.State] = M.S
      implicit def applicativeAsk[M[_]](implicit M: TypeInference[M]): ApplicativeAsk[M, TypeContext] = M.A
      implicit def applicativeLocal[M[_]](implicit M: TypeInference[M]): ApplicativeLocal[M, TypeContext] = M.L
    }

    type TypeContext = Map[String, TypeBinding]

    implicit class BindingContextOps[M[_]](ctx: TypeContext) {
      def getBinding(name: String)(implicit TI: TypeInference[M]): M[Identifier] = {
        ctx.get(name) match {
          case None      => TI.E.raise(s"Unable to find type binding for variable $name")
          case Some(tis) => tis.get[M]
        }
      }
    }

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
    val constantQualifiedTypes: TypeContext =
      Constant.values
        .map(constant => constant.entryName -> TypeBinding.Predefined(constant.entryName, constant.qualifiedType))
        .toMap

    def findConstraints[M[_]](expr: AST)(implicit TI: TypeInference[M]): M[Constraints] = {
      import TypeInference._
      val nodeConstraints: M[Constraints] = expr match {
        case Identifier(_, qt, _) => Constraints.empty.withPredicates(qt.predicates).pure[M]
        // TODO predicates
        case Number(_, tpe) => Constraints.empty.withPredicate(Predicate("Num", List(tpe.t))).pure[M]
        case Bool(_, tpe)   => Constraints(tpe.t -> Type.Bool).pure[M]
        case App(f, x, tpe) => Constraints(f.tpe.t -> (x.tpe.t =>: tpe.t)).pure[M]
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
            case _                  => s"$head constraint can't be unified".raise[M, Unification]
          }
      }

    class PredicatesUnifier[M[_]] {
      import TypeInferenceInstances._
      def unify(defaults: List[Default], instances: List[Predicate], predicates: List[Predicate]): Option[Substitution] = {
        val predicateToSubstitutions: Map[Predicate, List[Substitution]] =
          predicates.distinct.map { predicate => predicate ->
            instances.flatMap(instance => matchPredicateWithInstance(instance, predicate))
          }.toMap

        val reducedPredicateToSubstitutions = predicateToSubstitutions.filterNot {
          case (_, substitutions) => hasEmptySubstitution(substitutions)
        }

        product(reducedPredicateToSubstitutions.values.toList).flatMap(mergeSubstitutions).headOption
      }

      private def hasEmptySubstitution(substitutions: List[Substitution]): Boolean =
        substitutions.contains(Substitution.empty)

      private def matchPredicateWithInstance(instance: Predicate, predicate: Predicate): Option[Substitution] =
        (instance, predicate) match {
          case (Predicate(iId, iTypes), Predicate(pId, pTypes)) if iId == pId => matchTypes(iTypes, pTypes)
          case _ => None
        }

      private def matchTypes(instTypes: List[Type], predTypes: List[Type]): Option[Substitution] =
        (instTypes, predTypes) match {
          case (iHead :: iTail, pHead :: pTail) => for {
            tailSubst <- matchTypes(iTail, pTail)
            headSubst <- matchType(iHead, pHead)
            subst <- headSubst.merge[Either[String, ?]](tailSubst).toOption
          } yield subst

          case (Nil, Nil) => Some(Substitution.empty)
          case _ => None
        }

      private def matchType(instType: Type, predType: Type): Option[Substitution] = (instType, predType) match {
        case (t1, Type.Var(name)) => Some(Substitution(name -> t1))
        case (t1, t2) if t1 == t2 => Some(Substitution.empty)
        case _ => None
      }

      private def product[T](lists: List[List[T]]): List[List[T]] =
        lists match {
          case firstList :: otherLists => 
            for {
              otherTs <- product(otherLists)
              t <- firstList
            } yield t :: otherTs
          case Nil => List(Nil)
        }

      private def mergeSubstitutions(substitutions: List[Substitution]): Option[Substitution] =
        substitutions match {
          case substHead :: substTail => mergeSubstitutions(substTail).flatMap(_.merge[Either[String, ?]](substHead).toOption)
          case Nil => Some(Substitution.empty)
        }
    }

    // TODO: Very, Very naive typeclass checking, that works for now because we just have typeclasses without derivation
    def predicatesSubstitution[M[_]](predicates: List[Predicate])(implicit M: TypeInference[M]): M[Substitution] = {
      import TypeInferenceInstances._
      // num literals are qualified with a Type.Var(x) -> Predicate(Num, List(Type.Var(x))
      // we don't want to resolve them too early with the first instance subst.
      // At the moment I don't know what to do with ambiguous subst.
      // Functional Dependencies?
      val predicatesWithNonVars = predicates.filter(p => p.types.exists(tpe => typeVars(tpe).isEmpty))
      val substitution: Substitution = substForMultiplePredicates(predicatesWithNonVars)
      val substitutedPredicates = substitution.substitute(predicates)
      val substitutedPredicatesWithNonVars: List[Predicate] =
        substitutedPredicates.filter(p => p.types.exists(tpe => typeVars(tpe).isEmpty))
      val invalidPredicates = substitutedPredicatesWithNonVars.filter(p => !instances.contains(p))
      if (invalidPredicates.isEmpty) substitution.pure[M]
      else s"Not found instances for predicates $invalidPredicates".raise[M, Substitution]
    }

    private def substForMultiplePredicates(predicates: List[Predicate]): Substitution =
      predicates match {
        case Nil          => Substitution.empty
        case head :: tail => substForMultiplePredicates(tail).compose(substForInstances(head, instances))
      }

    private def substForInstances(predicate: Predicate, instances: List[Predicate]): Substitution = {
      val relevantInstances: List[Predicate] = instances.filter(_.id == predicate.id)
      relevantInstances match {
        case head :: tail => substitutionForPredicate(predicate, head).compose(substForInstances(predicate, tail))
        case Nil          => Substitution.empty
      }
    }

    private def substitutionForPredicate(predicate1: Predicate, predicate2: Predicate): Substitution =
      (predicate1, predicate2) match {
        case (Predicate(name1, _), Predicate(name2, _)) if name1 != name2               => Substitution.empty
        case (Predicate(_, types1), Predicate(_, types2)) if types1.size != types2.size => Substitution.empty
        case (Predicate(_, types1), Predicate(_, types2)) =>
          substitutionForPredicateTypes(types1, types2).getOrElse(Substitution.empty)
      }

    private def substitutionForPredicateTypes(types1: List[Type], types2: List[Type]): Option[Substitution] =
      (types1, types2) match {
        case (Type.Var(x) :: tail1, t2 :: tail2) =>
          substitutionForPredicateTypes(tail1, tail2).map(_.compose(Substitution(x -> t2)))
        case (t1 :: tail1, Type.Var(x) :: tail2) =>
          substitutionForPredicateTypes(tail1, tail2).map(_.compose(Substitution(x -> t1)))
        case (t1 :: _, t2 :: _) if t1 != t2 => None
        case (_ :: tail1, _ :: tail2)       => substitutionForPredicateTypes(tail1, tail2)
        case (Nil, Nil)                     => Some(Substitution.empty)
        case _                              => None
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

    sealed abstract class TypeBinding(val name: String, val qt: Qualified[Type]) {
      import TypeInference._
      def get[M[_]](implicit TI: TypeInference[M]): M[Identifier] = {
        this match {
          case TypeBinding.Variable(_, _) => Identifier(name, qt).pure[M]
          case TypeBinding.Predefined(_, _) =>
            val varsInScheme = typeVars(qt.t).toList
            for {
              assignments <- varsInScheme.traverse(
                schemeVar => newTypeVar.map(typeVar => Assignment(schemeVar.name, typeVar.t))
              )
              substitution = Substitution(assignments)
            } yield Identifier(name, substitution.substitute(qt), primitive = true)
        }
      }
    }
    object TypeBinding {
      case class Variable(override val name: String, override val qt: Qualified[Type]) extends TypeBinding(name, qt)
      case class Predefined(override val name: String, override val qt: Qualified[Type]) extends TypeBinding(name, qt)
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

    final case class Assignment(variable: String, tpe: Type)

    final case class Substitution(assignments: List[Assignment]) {
      def lookup(variable: String): Option[Type] = assignments.find(_.variable == variable).map(_.tpe)
      def substitute[T](t: T)(implicit cbs: CanBeSubstituted[T]): T = cbs.substitute(this, t)
      def compose(s2: Substitution): Substitution = Substitution(substitute(s2).assignments ++ assignments)
      def merge[M[_]](s2: Substitution)(implicit M: FunctorRaise[M, String], A: Applicative[M]): M[Substitution] = {
        val commonVars = assignments.map(_.variable).intersect(s2.assignments.map(_.variable))
        val agree = commonVars.forall { variable =>
          substitute[Type](Type.Var(variable)) == s2.substitute[Type](Type.Var(variable))
        }
        if (agree) Substitution(assignments ++ s2.assignments).pure[M]
        else M.raise("Merge has failed")
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

    /**
     * TODO A lot of coupling between this, All the instances, and Typeclass extraction in Types Module
     */
    val instances: List[Predicate] = List(
      Predicate("Num", List(Type.Dbl)),
      Predicate("Num", List(Type.Integer)),
      Predicate("Semigroup", List(Type.Integer)),
      Predicate("Semigroup", List(Type.Dbl)),
      Predicate("Semigroup", List(Type.Point)),
      Predicate("LeftModule", List(Type.Dbl, Type.Dbl)),
      Predicate("LeftModule", List(Type.Dbl, Type.Point)),
      Predicate("LeftModule", List(Type.Integer, Type.Integer)),
      Predicate("LeftModule", List(Type.Integer, Type.Dbl)),
      Predicate("LeftModule", List(Type.Integer, Type.Point)),
      Predicate("LeftModule", List(Type.Dbl, Type.Evo(Type.Point)))
    )
  }
}
