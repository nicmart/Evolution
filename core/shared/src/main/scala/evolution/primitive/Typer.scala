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

  def findConstraints(expr: Expr): List[Constraint] = (expr match {
    case Expr.Var(_, _) => Nil
    case Expr.FuncCall(funcName, args, tpe) =>
      (funcName, args) match {
        case ("point", x :: y :: Nil) =>
          List(Constraint(tpe, Type.Point), Constraint(x.tpe, Type.Dbl), Constraint(y.tpe, Type.Dbl))
      }
    case Expr.Lambda(varName, expr, tpe) => Nil
    case Expr.Number(n, tpe)             => Nil
  }) ++ expr.children.flatMap(findConstraints)

  case class Constraint(a: Type, b: Type)

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
