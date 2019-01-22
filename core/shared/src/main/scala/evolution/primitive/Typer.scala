package evolution.primitive

import scala.util.Try

class Typer[F[_]](val ast: Ast[F]) {
  import ast._
  import Expr._, PredefinedFunction._

  /**
   * Traverse the AST and assign type variables to each expression.
   * No constraint is added at this stage
   */
  def assignVars(expr: Expr): Expr =
    assignVarsRec(TypeVars.empty, expr)._2

  def findConstraints(expr: Expr): Constraints = (expr match {
    case Var(_, _) => Constraints.empty
    case FuncCall(funcName, args, tpe) =>
      (funcName, args) match {
        case (Point, x :: y :: Nil) =>
          Constraints(tpe -> Type.Point, x.tpe -> Type.Dbl, y.tpe -> Type.Dbl)
        case (App, f :: x :: Nil) =>
          Constraints(f.tpe -> Type.Arrow(x.tpe, tpe))
      }
    case Lambda(variable, lambdaExpr, tpe) =>
      val arrowConstraint = Constraints(tpe -> Type.Arrow(variable.tpe, lambdaExpr.tpe))
      val variableConstraints = Constraints(varUsagesIn(variable.name, lambdaExpr).map(u => u.tpe -> variable.tpe): _*)
      arrowConstraint.merge(variableConstraints)
    case Number(n, tpe) => Constraints.empty
    case _              => ???
  }).merge(expr.children.map(findConstraints))

  def varUsagesIn(varName: String, expr: Expr): List[Expr] =
    expr match {
      case Var(name, _) if name == varName                                  => List(expr)
      case Var(_, _)                                                        => Nil
      case FuncCall(funcName, args, _)                                      => args.flatMap(varUsagesIn(varName, _))
      case Lambda(Var(lambdaVar, _), lambdaExpr, _) if lambdaVar == varName => Nil // Shadowing
      case Lambda(_, lambdaExpr, _)                                         => varUsagesIn(varName, lambdaExpr)
      case Number(n, _)                                                     => Nil
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

  private def assignVarsRec(vars: TypeVars, expr: Expr): (TypeVars, Expr) =
    expr match {
      case Var(name, _) =>
        vars.withNext(expr.withType)

      case Number(n, _) =>
        vars.withNext(expr.withType)

      case FuncCall(funcName, funcArgs, _) =>
        val (typeVarsWithArgsVars, transformedArgs) =
          funcArgs.foldLeft[(TypeVars, List[Expr])]((vars, Nil)) {
            case ((typeVarsSoFar, typedArgsSoFar), arg) =>
              val (nextTypeVars, typedArg) = assignVarsRec(typeVarsSoFar, arg)
              (nextTypeVars, typedArg :: typedArgsSoFar)
          }
        typeVarsWithArgsVars.withNext(next => FuncCall(funcName, transformedArgs.reverse, next))

      case Lambda(varName, lambdaBody, _) =>
        val (vars1, typedVar) = assignVarsRec(vars, varName)
        val (vars2, typedBody) = assignVarsRec(vars1, lambdaBody)
        vars2.withNext(next => Lambda(varName.copy(tpe = typedVar.tpe), typedBody, next))
    }

  private class TypeVars(total: Int) {
    def withNext[T](f: Type.Var => T): (TypeVars, T) =
      (new TypeVars(total + 1), f(Type.Var(s"T$total")))
  }

  private object TypeVars {
    val empty = new TypeVars(0)
  }

  private def parseInt(n: String): Either[String, Int] =
    Try(n.toInt).toEither.left.map(_ => "")

  private def parseDouble(n: String): Either[String, Double] =
    Try(n.toDouble).toEither.left.map(_ => "")
}
