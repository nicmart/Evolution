package evolution.primitive

import scala.util.Try

trait TyperModule[F[_]] { self: WithAst[F] =>
  import ast._
  import Expr._, PredefinedFunction._

  object Typer {

    /**
     * Traverse the AST and assign type variables to each expression.
     * No constraint is added at this stage
     */
    def assignVars(vars: TypeVars, expr: Expr): (TypeVars, Expr) =
      expr match {
        case _ if expr.tpe != Type.Var("") =>
          (vars, expr)
        case Var(name, _) =>
          vars.withNext(expr.withType)

        case Number(n, _) =>
          vars.withNext(expr.withType)

        case FuncCall(funcName, funcArgs, _) =>
          val (typeVarsWithArgsVars, transformedArgs) =
            vars.traverse(funcArgs)(assignVars)
          typeVarsWithArgsVars.withNext(next => FuncCall(funcName, transformedArgs, next))

        case Lambda(varName, lambdaBody, _) =>
          val (vars1, typedVar) = assignVars(vars, varName)
          val (vars2, typedBody) = assignVars(vars1, lambdaBody)
          vars2.withNext(next => Lambda(varName.copy(tpe = typedVar.tpe), typedBody, next))

        case Let(varName, value, in, _) =>
          val (vars1, typedVar) = assignVars(vars, varName)
          val (vars2, typedValue) = assignVars(vars1, value)
          val (vars3, typedIn) = assignVars(vars2, in)
          vars3.withNext(next => Let(varName.copy(tpe = typedVar.tpe), typedValue, typedIn, next))
      }

    def findConstraints(typeVars: TypeVars, expr: Expr): (TypeVars, Constraints) = {
      val (vars1, constraints1) = expr match {
        case Var(_, _)              => (typeVars, Constraints.empty)
        case fc @ FuncCall(_, _, _) => findConstraintsOfPredefinedFunctionCall(typeVars, fc)
        case Lambda(variable, lambdaExpr, tpe) =>
          val arrowConstraint = Constraints(tpe -> Type.Arrow(variable.tpe, lambdaExpr.tpe))
          val variableConstraints =
            Constraints(varUsagesIn(variable.name, lambdaExpr).map(u => u.tpe -> variable.tpe): _*)
          (typeVars, arrowConstraint.merge(variableConstraints))
        case Let(variable, value, in, tpe) =>
          (
            typeVars,
            Constraints(varUsagesIn(variable.name, in).map(u => u.tpe -> variable.tpe): _*)
              .merge(Constraints(variable.tpe -> value.tpe, in.tpe -> tpe)))
        case Number(n, tpe) => (typeVars, Constraints.empty)
        case _              => ???
      }
      val (vars2, constraints2) = vars1.traverse(expr.children)(findConstraints)
      (vars2, constraints1.merge(constraints2))
    }

    def assignVarsAndFindConstraints(expr: Expr): (Expr, Constraints) = {
      val (vars, exprWithVars) = assignVars(TypeVars.empty, expr)
      (exprWithVars, findConstraints(vars, exprWithVars)._2)
    }

    def unify(constraints: Constraints): Either[String, Substitution] =
      constraints.constraints match {
        case Nil => Right(Substitution.Empty)
        case head :: tail =>
          head match {
            case Constraint(a, b) if a == b => unify(Constraints(tail))
            case Constraint(Type.Var(x), t) if typeVarUsagesIn(x, t).isEmpty =>
              val substituteVar = Substitution.Simple(x, t)
              val constraints2 = substituteVar.substitute(Constraints(tail))
              unify(constraints2).map(subst => Substitution.Composite(substituteVar, subst))
            case Constraint(t, Type.Var(x)) if typeVarUsagesIn(x, t).isEmpty =>
              val substituteVar = Substitution.Simple(x, t)
              val constraints2 = substituteVar.substitute(Constraints(tail))
              unify(constraints2).map(subst => Substitution.Composite(substituteVar, subst))
            case Constraint(Type.Evo(a), Type.Evo(b)) =>
              unify(Constraints(a -> b).merge(Constraints(tail)))
            case Constraint(Type.Arrow(a1, b1), Type.Arrow(a2, b2)) =>
              unify(Constraints(a1 -> a2, b1 -> b2).merge(Constraints(tail)))
            case _ => Left(s"$head constraint can't be unified")
          }
      }

    private def findConstraintsOfPredefinedFunctionCall(typeVars: TypeVars, func: FuncCall): (TypeVars, Constraints) =
      (func.funcId, func.args) match {
        case (Point, x :: y :: Nil) =>
          (typeVars, Constraints(func.tpe -> Type.Point, x.tpe -> Type.Dbl, y.tpe -> Type.Dbl))
        case (Add, x :: y :: Nil) =>
          (typeVars, Constraints(x.tpe -> y.tpe, func.tpe -> x.tpe))
        case (Inverse, x :: Nil) =>
          (typeVars, Constraints(x.tpe -> func.tpe))
        case (Multiply, x :: y :: Nil) =>
          (typeVars, Constraints(x.tpe -> Type.Dbl, y.tpe -> func.tpe))
        case (Cos, x :: Nil) =>
          (typeVars, Constraints(x.tpe -> Type.Dbl, func.tpe -> Type.Dbl))
        case (Sin, x :: Nil) =>
          (typeVars, Constraints(x.tpe -> Type.Dbl, func.tpe -> Type.Dbl))
        case (Eq, x :: y :: Nil) =>
          (typeVars, Constraints(x.tpe -> y.tpe, func.tpe -> Type.Bool))
        case (If, x :: y :: z :: Nil) =>
          (typeVars, Constraints(x.tpe -> Type.Bool, y.tpe -> z.tpe, func.tpe -> y.tpe))
        case (Fix, f :: Nil) =>
          (typeVars, Constraints(f.tpe -> Type.Arrow(func.tpe, func.tpe)))
        case (App, f :: x :: Nil) =>
          (typeVars, Constraints(f.tpe -> Type.Arrow(x.tpe, func.tpe)))
        case (Empty, Nil) =>
          typeVars.withNext(v => Constraints(func.tpe -> Type.Evo(v)))
        case (Cons, x :: y :: Nil) => // TODO I am not sure if we can assume transitivity and remove redundant constraints
          (typeVars, Constraints(func.tpe -> Type.Evo(x.tpe), y.tpe -> Type.Evo(x.tpe), func.tpe -> Type.Evo(x.tpe)))
        case (MapEmpty, x :: y :: Nil) =>
          (typeVars, Constraints(func.tpe -> x.tpe, func.tpe -> y.tpe, x.tpe -> y.tpe))
        case (MapCons, x :: f :: Nil) =>
          typeVars.withNext2 { (a, b) =>
            Constraints(
              x.tpe -> Type.Evo(a),
              func.tpe -> Type.Evo(b),
              f.tpe -> Type.Arrow(a, Type.Arrow(Type.Evo(a), Type.Evo(b)))
            )
          }

        case (Cartesian, x :: y :: Nil) =>
          (
            typeVars,
            Constraints(func.tpe -> Type.Evo(Type.Point), x.tpe -> Type.Evo(Type.Dbl), y.tpe -> Type.Evo(Type.Dbl)))
        case (Polar, x :: y :: Nil) =>
          (
            typeVars,
            Constraints(func.tpe -> Type.Evo(Type.Point), x.tpe -> Type.Evo(Type.Dbl), y.tpe -> Type.Evo(Type.Dbl)))
        case (Constant, x :: Nil) =>
          (typeVars, Constraints(func.tpe -> Type.Evo(x.tpe)))
        case (Integrate, x :: y :: Nil) =>
          (typeVars, Constraints(func.tpe -> Type.Evo(x.tpe), y.tpe -> Type.Evo(x.tpe))) // Here I omitted the redundant condition)
        case (Solve1, x :: y :: Nil) =>
          (typeVars, Constraints(func.tpe -> Type.Evo(y.tpe), x.tpe -> Type.Evo(Type.Arrow(x.tpe, x.tpe))))
        case (Solve2, x :: y :: z :: Nil) =>
          (
            typeVars,
            Constraints(
              y.tpe -> x.tpe,
              func.tpe -> Type.Evo(x.tpe),
              x.tpe -> Type.Evo(Type.Arrow(x.tpe, Type.Arrow(x.tpe, x.tpe)))))
        case (Concat, x :: y :: Nil) => // TODO gen new vars
          typeVars.withNext { a =>
            Constraints(
              x.tpe -> Type.Evo(a),
              y.tpe -> Type.Evo(a),
              func.tpe -> Type.Evo(a),
            )
          }
        case (Map, x :: f :: Nil) =>
          typeVars.withNext2 { (a, b) =>
            Constraints(
              x.tpe -> Type.Evo(a),
              func.tpe -> Type.Evo(b),
              f.tpe -> Type.Arrow(a, b)
            )
          }
        case (FlatMap, x :: f :: Nil) =>
          typeVars.withNext2 { (a, b) =>
            Constraints(
              x.tpe -> Type.Evo(a),
              func.tpe -> Type.Evo(b),
              f.tpe -> Type.Arrow(a, Type.Evo(b))
            )
          }
        case (Take, n :: e :: Nil) =>
          typeVars.withNext { a =>
            Constraints(
              n.tpe -> Type.Integer,
              e.tpe -> Type.Evo(a),
              func.tpe -> Type.Evo(a),
            )
          }

        case (Uniform, from :: to :: Nil) =>
          (typeVars, Constraints(func.tpe -> Type.Evo(Type.Dbl), from.tpe -> Type.Dbl, to.tpe -> Type.Dbl))
      }

    def varUsagesIn(varName: String, expr: Expr): List[Expr] =
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

    case class Constraint(a: Type, b: Type) {
      def reverse: Constraint = Constraint(b, a)
      def isEquivalentTo(other: Constraint): Boolean = this == other || this == other.reverse
    }

    case class Constraints(constraints: List[Constraint]) {
      def merge(other: Constraints): Constraints = Constraints(constraints ++ other.constraints)
      def merge(other: List[Constraints]): Constraints = other.foldLeft(this) { (constraints, current) =>
        constraints.merge(current)
      }
      def isEquivalentTo(other: Constraints): Boolean = ???
      override def toString: String = constraints.map(c => s"${c.a} = ${c.b}").mkString("\n")
    }

    object Constraints {
      val empty: Constraints = Constraints(Nil)
      def apply(constraints: (Type, Type)*): Constraints = Constraints(constraints.toList.map {
        case (a, b) => Constraint(a, b)
      })
    }

    sealed trait Substitution {
      def substitute(t: Type): Type

      final def substitute(c: Constraint): Constraint =
        Constraint(substitute(c.a), substitute(c.b))

      final def substitute(c: Constraints): Constraints =
        Constraints(c.constraints.map(substitute))

      final def substitute(expr: Expr): Expr =
        expr match {
          case variable @ Expr.Var(name, tpe)   => substitute(variable)
          case Expr.FuncCall(funcId, args, tpe) => Expr.FuncCall(funcId, args.map(substitute), substitute(tpe))
          case Expr.Lambda(varName, lambdaExpr, tpe) =>
            Expr.Lambda(substitute(varName), substitute(lambdaExpr), substitute(tpe))
          case Expr.Number(n, tpe) => Expr.Number(n, substitute(tpe))
        }

      final def substitute(expr: Expr.Var): Expr.Var =
        expr match {
          case Expr.Var(name, tpe) => Expr.Var(name, substitute(tpe))
        }

      final def andThen(other: Substitution): Substitution =
        Substitution.Composite(this, other)
    }

    object Substitution {
      final case class Simple(variable: String, withType: Type) extends Substitution {
        override def substitute(t: Type): Type = t match {
          case Type.Var(name) if name == variable => withType
          case Type.Evo(inner)                    => Type.Evo(substitute(inner))
          case Type.Arrow(from, to)               => Type.Arrow(substitute(from), substitute(to))
          case Type.Var(name)                     => t
          case Type.Integer                       => t
          case Type.Dbl                           => t
          case Type.Point                         => t
          case Type.Bool                          => t
        }
      }

      final case class Composite(a: Substitution, b: Substitution) extends Substitution {
        override def substitute(t: Type): Type = b.substitute(a.substitute(t))
      }

      final case object Empty extends Substitution {
        override def substitute(t: Type): Type = t
      }
    }

    class TypeVars(total: Int) {
      def withNext[T](f: Type.Var => T): (TypeVars, T) =
        (new TypeVars(total + 1), f(Type.Var(s"T$total")))
      def withNext2[T](f: (Type.Var, Type.Var) => T): (TypeVars, T) = {
        val (vars1, var1) = withNext(identity)
        val (vars2, var2) = vars1.withNext(identity)
        (vars2, f(var1, var2))
      }
      def traverse[A, B](ts: List[A])(f: (TypeVars, A) => (TypeVars, B)): (TypeVars, List[B]) =
        ts.foldLeft[(TypeVars, List[B])]((this, Nil)) {
          case ((accVars, bs), a) =>
            val (newVars, b) = f(accVars, a)
            (newVars, bs :+ b)
        }
    }

    object TypeVars {
      val empty = new TypeVars(0)
    }

    private def parseInt(n: String): Either[String, Int] =
      Try(n.toInt).toEither.left.map(_ => "")

    private def parseDouble(n: String): Either[String, Double] =
      Try(n.toDouble).toEither.left.map(_ => "")
  }
}
