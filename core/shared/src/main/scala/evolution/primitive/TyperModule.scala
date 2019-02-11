package evolution.primitive

import cats.data.State
import cats.{ Monad, MonadError }
import cats.implicits._

trait TyperModule[F[_]] { self: WithAst[F] =>
  import ast._
  import AST._, PredefinedFunction._

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
     * Traverse the AST and assign type variables to each expression.
     * No constraint is added at this stage
     */
    def assignVars(expr: AST): TypeInference[AST] =
      expr match {
        case _ if expr.tpe != Type.Var("") =>
          State.pure(expr)

        case Var(name, _) =>
          newVar.map(expr.withType)

        case Number(n, _) =>
          newVar.map(expr.withType)

        case FuncCall(funcName, funcArgs, _) =>
          (funcArgs.traverse(assignVars), newVar).mapN { (transformedArgs, variable) =>
            FuncCall(funcName, transformedArgs, variable)
          }

        case Lambda(varName, lambdaBody, _) =>
          (assignVars(varName), assignVars(lambdaBody), newVar).mapN { (v, b, t) =>
            Lambda(varName.copy(tpe = v.tpe), b, t)
          }

        case Let(varName, value, in, _) =>
          (assignVars(varName), assignVars(value), assignVars(in), newVar).mapN { (tVar, tValue, tIn, tLet) =>
            Let(varName.copy(tpe = tVar.tpe), tValue, tIn, tLet)
          }
      }

    def findConstraints(expr: AST): TypeInference[Constraints] = {
      val nodeConstraints: TypeInference[Constraints] = expr match {
        case Var(_, _)              => State.pure(Constraints.empty)
        case fc @ FuncCall(_, _, _) => findConstraintsOfPredefinedFunctionCall(fc)
        case Lambda(variable, lambdaExpr, tpe) =>
          val arrowConstraint = Constraints(tpe -> Type.Arrow(variable.tpe, lambdaExpr.tpe))
          val variableConstraints =
            Constraints(varUsagesIn(variable.name, lambdaExpr).map(u => u.tpe -> variable.tpe): _*)
          arrowConstraint.merge(variableConstraints).pure[TypeInference]
        case Let(variable, value, in, tpe) =>
          Constraints(varUsagesIn(variable.name, in).map(u => u.tpe -> variable.tpe): _*)
            .merge(Constraints(variable.tpe -> value.tpe, in.tpe -> tpe))
            .pure[TypeInference]

        case Number(n, tpe) => Constraints.empty.pure[TypeInference]
        case _              => ???
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

    private def findConstraintsOfPredefinedFunctionCall(func: FuncCall): TypeInference[Constraints] =
      (func.funcId, func.args) match {

        case (Point, x :: y :: Nil) =>
          Constraints(func.tpe -> Type.Point, x.tpe -> Type.Dbl, y.tpe -> Type.Dbl).pure[TypeInference]

        case (X, p :: Nil) =>
          Constraints(func.tpe -> Type.Dbl, p.tpe -> Type.Point).pure[TypeInference]

        case (Y, p :: Nil) =>
          Constraints(func.tpe -> Type.Dbl, p.tpe -> Type.Point).pure[TypeInference]

        case (Floor, d :: Nil) =>
          Constraints(func.tpe -> Type.Integer, d.tpe -> Type.Dbl).pure[TypeInference]

        case (Add, x :: y :: Nil) =>
          Constraints(x.tpe -> y.tpe, func.tpe -> x.tpe).pure[TypeInference]

        case (Div, x :: y :: Nil) =>
          Constraints(func.tpe -> Type.Dbl, x.tpe -> Type.Dbl, y.tpe -> Type.Dbl).pure[TypeInference]

        case (Exp, x :: y :: Nil) =>
          Constraints(func.tpe -> Type.Dbl, x.tpe -> Type.Dbl, y.tpe -> Type.Dbl).pure[TypeInference]

        case (Inverse, x :: Nil) =>
          Constraints(x.tpe -> func.tpe).pure[TypeInference]

        case (Multiply, x :: y :: Nil) =>
          Constraints(x.tpe -> Type.Dbl, y.tpe -> func.tpe).pure[TypeInference]

        case (Cos, x :: Nil) =>
          Constraints(x.tpe -> Type.Dbl, func.tpe -> Type.Dbl).pure[TypeInference]

        case (Sin, x :: Nil) =>
          Constraints(x.tpe -> Type.Dbl, func.tpe -> Type.Dbl).pure[TypeInference]

        case (PI, Nil) =>
          Constraints(func.tpe -> Type.Dbl).pure[TypeInference]

        case (Mod, x :: y :: Nil) =>
          Constraints(func.tpe -> Type.Dbl, x.tpe -> Type.Dbl, y.tpe -> Type.Dbl).pure[TypeInference]

        case (Eq, x :: y :: Nil) =>
          Constraints(x.tpe -> y.tpe, func.tpe -> Type.Bool).pure[TypeInference]

        case (If, x :: y :: z :: Nil) =>
          Constraints(x.tpe -> Type.Bool, y.tpe -> z.tpe, func.tpe -> y.tpe).pure[TypeInference]

        case (Fix, f :: Nil) =>
          Constraints(f.tpe -> Type.Arrow(func.tpe, func.tpe)).pure[TypeInference]

        case (App, f :: x :: Nil) =>
          Constraints(f.tpe -> Type.Arrow(x.tpe, func.tpe)).pure[TypeInference]

        case (Empty, Nil) =>
          newVar.map(v => Constraints(func.tpe -> Type.Evo(v)))

        case (Cons, x :: y :: Nil) =>
          Constraints(func.tpe -> Type.Evo(x.tpe), y.tpe -> Type.Evo(x.tpe), func.tpe -> Type.Evo(x.tpe))
            .pure[TypeInference]

        case (MapEmpty, x :: y :: Nil) =>
          Constraints(func.tpe -> x.tpe, func.tpe -> y.tpe, x.tpe -> y.tpe).pure[TypeInference]

        case (MapCons, x :: f :: Nil) =>
          (newVar, newVar).mapN { (a, b) =>
            Constraints(
              x.tpe -> Type.Evo(a),
              func.tpe -> Type.Evo(b),
              f.tpe -> Type.Arrow(a, Type.Arrow(Type.Evo(a), Type.Evo(b)))
            )
          }

        case (Cartesian, x :: y :: Nil) =>
          Constraints(func.tpe -> Type.Evo(Type.Point), x.tpe -> Type.Evo(Type.Dbl), y.tpe -> Type.Evo(Type.Dbl))
            .pure[TypeInference]

        case (Polar, x :: y :: Nil) =>
          Constraints(func.tpe -> Type.Evo(Type.Point), x.tpe -> Type.Evo(Type.Dbl), y.tpe -> Type.Evo(Type.Dbl))
            .pure[TypeInference]

        case (Constant, x :: Nil) =>
          Constraints(func.tpe -> Type.Evo(x.tpe)).pure[TypeInference]

        case (Integrate, x :: y :: Nil) =>
          Constraints(func.tpe -> Type.Evo(x.tpe), y.tpe -> Type.Evo(x.tpe)).pure[TypeInference]

        case (Solve1, eq :: x :: Nil) =>
          Constraints(func.tpe -> Type.Evo(x.tpe), eq.tpe -> Type.Evo(Type.Arrow(x.tpe, x.tpe))).pure[TypeInference]

        case (Solve2, eq :: x :: y :: Nil) =>
          Constraints(
            func.tpe -> Type.Evo(x.tpe),
            x.tpe -> y.tpe,
            eq.tpe -> Type.Evo(Type.Arrow(x.tpe, Type.Arrow(x.tpe, x.tpe)))
          ).pure[TypeInference]

        case (Concat, x :: y :: Nil) => // TODO gen new vars
          newVar.map { a =>
            Constraints(
              x.tpe -> Type.Evo(a),
              y.tpe -> Type.Evo(a),
              func.tpe -> Type.Evo(a)
            )
          }

        case (Map, x :: f :: Nil) =>
          (newVar, newVar).mapN { (a, b) =>
            Constraints(
              x.tpe -> Type.Evo(a),
              func.tpe -> Type.Evo(b),
              f.tpe -> Type.Arrow(a, b)
            )
          }

        case (FlatMap, x :: f :: Nil) =>
          (newVar, newVar).mapN { (a, b) =>
            Constraints(
              x.tpe -> Type.Evo(a),
              func.tpe -> Type.Evo(b),
              f.tpe -> Type.Arrow(a, Type.Evo(b))
            )
          }
        case (Take, n :: e :: Nil) =>
          newVar.map { a =>
            Constraints(
              n.tpe -> Type.Integer,
              e.tpe -> Type.Evo(a),
              func.tpe -> Type.Evo(a)
            )
          }

        case (ZipWith, a :: b :: f :: Nil) =>
          (newVar, newVar, newVar).mapN { (typeOfA, typeOfB, typeOfResult) =>
            Constraints(
              a.tpe -> Type.Evo(typeOfA),
              b.tpe -> Type.Evo(typeOfB),
              func.tpe -> Type.Evo(typeOfResult),
              f.tpe -> Type.Arrow(typeOfA, Type.Arrow(typeOfB, typeOfResult))
            )
          }

        case (Uniform, from :: to :: Nil) =>
          Constraints(func.tpe -> Type.Evo(Type.Dbl), from.tpe -> Type.Dbl, to.tpe -> Type.Dbl).pure[TypeInference]

        case (UniformDiscrete, from :: to :: step :: Nil) =>
          Constraints(func.tpe -> Type.Evo(Type.Dbl), from.tpe -> Type.Dbl, to.tpe -> Type.Dbl, step.tpe -> Type.Dbl)
            .pure[TypeInference]

        case (UniformChoice, vars) =>
          Constraints(vars.map(v => func.tpe -> Type.Evo(v.tpe)): _*).pure[TypeInference]
      }

    def varUsagesIn(varName: String, expr: AST): List[AST] =
      expr match {
        case Var(name, _) if name == varName                         => List(expr)
        case Var(_, _)                                               => Nil
        case FuncCall(funcName, args, _)                             => args.flatMap(varUsagesIn(varName, _))
        case Lambda(Var(lambdaVar, _), _, _) if lambdaVar == varName => Nil // Shadowing
        case Lambda(_, lambdaExpr, _)                                => varUsagesIn(varName, lambdaExpr)
        case Let(Var(letVar, _), value, in, _) if letVar == varName  => varUsagesIn(varName, value) // Shadowing
        case Let(Var(_, _), value, in, _)                            => varUsagesIn(varName, value) ++ varUsagesIn(varName, in)
        case Number(n, _)                                            => Nil
      }

    def typeVarUsagesIn(varName: String, tpe: Type): List[Type] =
      tpe match {
        case Type.Var(name) if name == varName => List(tpe)
        case Type.Evo(inner)                   => typeVarUsagesIn(varName, inner)
        case Type.Arrow(from, to)              => typeVarUsagesIn(varName, from) ++ typeVarUsagesIn(varName, to)
        case _                                 => Nil
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
