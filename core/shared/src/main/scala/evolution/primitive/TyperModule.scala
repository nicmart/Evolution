package evolution.primitive

import cats.data.State
import cats.{ Monad, MonadError }
import cats.implicits._

trait TyperModule[F[_]] { self: WithAst[F] =>
  import ast._
  import AST._, PredefinedConstant._

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

        case Const(id, _) =>
          freshInstance(id.scheme).map(tpe => Const(id, tpe))

        case _ => // No-children expressions. Unsafe, that's why I would like to use transformChildren method
          newVar.map(expr.withType)
      }

    def findConstraints(expr: AST): TypeInference[Constraints] = {
      val nodeConstraints: TypeInference[Constraints] = expr match {
        case Var(_, _)      => Constraints.empty.pure[TypeInference]
        case Const(_, _)    => Constraints.empty.pure[TypeInference]
        case Number(_, _)   => Constraints.empty.pure[TypeInference]
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

    def unify[M[_]](constraints: Constraints)(implicit M: MonadError[M, String]): M[Substitution] =
      constraints.constraints match {
        case Nil => Substitution.empty.pure[M]
        case head :: tail =>
          head match {
            case Constraint(a, b) if a == b => unify[M](Constraints(tail))
            case Constraint(Type.Var(x), t) if typeVarUsagesIn(x, t).isEmpty =>
              val substituteVar = Substitution(x -> t)
              val constraints2 = substituteVar.substitute(Constraints(tail))
              unify[M](constraints2).map(_.compose(substituteVar))
            case Constraint(t, Type.Var(x)) if typeVarUsagesIn(x, t).isEmpty =>
              val substituteVar = Substitution(x -> t)
              val constraints2 = substituteVar.substitute(Constraints(tail))
              unify[M](constraints2).map(_.compose(substituteVar))
            case Constraint(Type.Evo(a), Type.Evo(b)) =>
              unify[M](Constraints(a -> b).merge(Constraints(tail)))
            case Constraint(Type.Arrow(a1, b1), Type.Arrow(a2, b2)) =>
              unify[M](Constraints(a1 -> a2, b1 -> b2).merge(Constraints(tail)))
            case _ => s"$head constraint can't be unified".raiseError[M, Substitution]
          }
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

    private def freshInstance(scheme: Type): TypeInference[Type] = {
      val varsInShceme = typeVars(scheme).toList
      for {
        assignments <- varsInShceme.traverse(schemeVar => newVar.map(typeVar => Assignment(schemeVar.name, typeVar)))
        substitution = Substitution(assignments)
      } yield substitution.substitute(scheme)
    }

    case class Constraint(a: Type, b: Type)

    case class Constraints(constraints: List[Constraint]) {
      def merge(other: Constraints): Constraints = Constraints(constraints ++ other.constraints)
      def merge(other: List[Constraints]): Constraints = other.foldLeft(this) { (constraints, current) =>
        constraints.merge(current)
      }
      override def toString: String = constraints.map(c => s"${c.a} = ${c.b}").mkString("\n")
    }

    object Constraints {
      val empty: Constraints = Constraints(Nil)
      def apply(constraints: (Type, Type)*): Constraints = Constraints(constraints.toList.map {
        case (a, b) => Constraint(a, b)
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
          Constraint(s.substitute(constraint.a), s.substitute(constraint.b))
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
  }
}
