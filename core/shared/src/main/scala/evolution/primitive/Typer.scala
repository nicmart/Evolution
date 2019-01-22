package evolution.primitive

import scala.util.Try

class Typer[F[_]](val ast: Ast[F]) {
  import ast._

  /**
   * Travers the AST and assign type variables to each expression.
   * No constraint is added at this stage
   */
  def assignVars(expr: Expr): Expr =
    assignVarsRec(TypeVars.empty, expr)._2

  def findConstraints(expr: Expr): Constraints = (expr match {
    case Expr.Var(_, _) => Constraints.empty
    case Expr.FuncCall(funcName, args, tpe) =>
      (funcName, args) match {
        case ("point", x :: y :: Nil) =>
          Constraints(tpe -> Type.Point, x.tpe -> Type.Dbl, y.tpe -> Type.Dbl)
      }
    case Expr.Lambda(varName, expr, tpe) => Constraints.empty
    case Expr.Number(n, tpe)             => Constraints.empty
  }).merge(expr.children.map(findConstraints))

  def varUsagesIn(varName: String, expr: Expr): List[Expr] =
    expr match {
      case Expr.Var(name, _) if name == varName                                       => List(expr)
      case Expr.Var(_, _)                                                             => Nil
      case Expr.FuncCall(funcName, args, _)                                           => args.flatMap(varUsagesIn(varName, _))
      case Expr.Lambda(Expr.Var(lambdaVar, _), lambdaExpr, _) if lambdaVar == varName => Nil // Shadowing
      case Expr.Lambda(lambdaVar, lambdaExpr, _)                                      => varUsagesIn(varName, lambdaExpr)
      case Expr.Number(n, _)                                                          => Nil
    }

  case class Constraint(a: Type, b: Type)
  case class Constraints(constraints: List[Constraint]) {
    def merge(other: Constraints): Constraints = Constraints(constraints ++ other.constraints)
    def merge(other: List[Constraints]): Constraints = other.foldLeft(this) { (constraints, current) =>
      constraints.merge(current)
    }
  }
  object Constraints {
    val empty: Constraints = Constraints(Nil)
    def apply(constraints: (Type, Type)*): Constraints = Constraints(constraints.toList.map {
      case (a, b) => Constraint(a, b)
    })
  }

  private def assignVarsRec(vars: TypeVars, expr: Expr): (TypeVars, Expr) =
    expr match {
      case Expr.Var(name, _) =>
        vars.withNext(expr.withType)

      case Expr.Number(n, _) =>
        vars.withNext(expr.withType)

      case Expr.FuncCall(funcName, funcArgs, _) =>
        val (typeVarsWithArgsVars, transformedArgs) =
          funcArgs.foldLeft[(TypeVars, List[Expr])]((vars, Nil)) {
            case ((typeVarsSoFar, typedArgsSoFar), arg) =>
              val (nextTypeVars, typedArg) = assignVarsRec(typeVarsSoFar, arg)
              (nextTypeVars, typedArg :: typedArgsSoFar)
          }
        typeVarsWithArgsVars.withNext(next => Expr.FuncCall(funcName, transformedArgs.reverse, next))

      case Expr.Lambda(varName, lambdaBody, _) =>
        val (vars1, typedVar) = assignVarsRec(vars, varName)
        val (vars2, typedBody) = assignVarsRec(vars1, lambdaBody)
        vars2.withNext(next => Expr.Lambda(varName.copy(tpe = typedVar.tpe), typedBody, next))
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
