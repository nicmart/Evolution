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
    case Var(_, _)              => Constraints.empty
    case fc @ FuncCall(_, _, _) => findConstraintsOfPredefinedFunctionCall(fc)
    case Lambda(variable, lambdaExpr, tpe) =>
      val arrowConstraint = Constraints(tpe -> Type.Arrow(variable.tpe, lambdaExpr.tpe))
      val variableConstraints = Constraints(varUsagesIn(variable.name, lambdaExpr).map(u => u.tpe -> variable.tpe): _*)
      arrowConstraint.merge(variableConstraints)
    case Number(n, tpe) => Constraints.empty
    case _              => ???
  }).merge(expr.children.map(findConstraints))

  private def findConstraintsOfPredefinedFunctionCall(func: FuncCall): Constraints =
    (func.funcId, func.args) match {
      case (Point, x :: y :: Nil) =>
        Constraints(func.tpe -> Type.Point, x.tpe -> Type.Dbl, y.tpe -> Type.Dbl)
      case (Add, x :: y :: Nil) =>
        Constraints(x.tpe -> y.tpe)
      case (Inverse, x :: Nil) =>
        Constraints.empty
      case (Multiply, x :: y :: Nil) =>
        Constraints(x.tpe -> y.tpe)
      case (Cos, x :: Nil) =>
        Constraints(x.tpe -> Type.Dbl, func.tpe -> Type.Dbl)
      case (Sin, x :: Nil) =>
        Constraints(x.tpe -> Type.Dbl, func.tpe -> Type.Dbl)
      case (Eq, x :: y :: Nil) =>
        Constraints(x.tpe -> y.tpe, func.tpe -> Type.Bool)
      case (If, x :: y :: z :: Nil) =>
        Constraints(x.tpe -> Type.Bool, y.tpe -> z.tpe, func.tpe -> y.tpe)
      case (Fix, f :: Nil) =>
        Constraints(f.tpe -> Type.Arrow(func.tpe, func.tpe))
      case (App, f :: x :: Nil) =>
        Constraints(f.tpe -> Type.Arrow(x.tpe, func.tpe))
      case (Empty, Nil) =>
        Constraints(func.tpe -> Type.Evo(Type.Var("Not used")))
      case (Cons, x :: y :: Nil) => // TODO I am not sure if we can assume transitivity and remove redundant constraints
        Constraints(func.tpe -> y.tpe, y.tpe -> Type.Evo(x.tpe), func.tpe -> Type.Evo(x.tpe))
      case (MapEmpty, x :: y :: Nil) =>
        Constraints(func.tpe -> x.tpe, func.tpe -> y.tpe, x.tpe -> y.tpe)
      case (MapCons, x :: f :: Nil) => // TODO we need to generate new type vars...
        Constraints(
          x.tpe -> Type.Evo(Type.Var("X")),
          func.tpe -> Type.Evo(Type.Var("Y")),
          f.tpe -> Type.Arrow(Type.Var("X"), Type.Arrow(Type.Evo(Type.Var("X")), Type.Evo(Type.Var("Y"))))
        )

      case (Cartesian, x :: y :: Nil) =>
        Constraints(func.tpe -> Type.Evo(Type.Point), x.tpe -> Type.Evo(Type.Dbl), y.tpe -> Type.Evo(Type.Dbl))
      case (Point, x :: y :: Nil) =>
        Constraints(func.tpe -> Type.Evo(Type.Point), x.tpe -> Type.Evo(Type.Dbl), y.tpe -> Type.Evo(Type.Dbl))
      case (Constant, x :: Nil) =>
        Constraints(func.tpe -> Type.Evo(x.tpe))
      case (Integrate, x :: y :: Nil) =>
        Constraints(func.tpe -> Type.Evo(x.tpe), y.tpe -> Type.Evo(x.tpe)) // Here I omitted the redundant condition
      case (Solve1, x :: y :: Nil) =>
        Constraints(func.tpe -> Type.Evo(y.tpe), x.tpe -> Type.Evo(Type.Arrow(x.tpe, x.tpe)))
      case (Solve2, x :: y :: z :: Nil) =>
        Constraints(
          y.tpe -> x.tpe,
          func.tpe -> Type.Evo(x.tpe),
          x.tpe -> Type.Evo(Type.Arrow(x.tpe, Type.Arrow(x.tpe, x.tpe))))
      case (Concat, x :: y :: Nil) => // TODO gen new vars
        Constraints(
          x.tpe -> Type.Evo(Type.Var("X")),
          y.tpe -> Type.Evo(Type.Var("X")),
          func.tpe -> Type.Evo(Type.Var("X")),
        )
      case (Map, x :: f :: Nil) => // TODO we need to generate new type vars
        Constraints(
          x.tpe -> Type.Evo(Type.Var("X")),
          func.tpe -> Type.Evo(Type.Var("Y")),
          f.tpe -> Type.Arrow(Type.Var("X"), Type.Var("Y"))
        )
      case (FlatMap, x :: f :: Nil) => // TODO we need to generate new type vars
        Constraints(
          x.tpe -> Type.Evo(Type.Var("X")),
          func.tpe -> Type.Evo(Type.Var("Y")),
          f.tpe -> Type.Arrow(Type.Var("X"), Type.Evo(Type.Var("Y")))
        )
      case (Take, n :: e :: Nil) => // TODO we need to generate new type vars
        Constraints(
          n.tpe -> Type.Integer,
          e.tpe -> Type.Evo(Type.Var("X")),
          func.tpe -> Type.Evo(Type.Var("X")),
        )

      case (Uniform, from :: to :: Nil) =>
        Constraints(func.tpe -> Type.Evo(Type.Dbl), from.tpe -> Type.Dbl, to.tpe -> Type.Dbl)
    }

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
