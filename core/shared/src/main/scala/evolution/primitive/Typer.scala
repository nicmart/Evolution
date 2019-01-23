package evolution.primitive

import scala.util.Try

class Typer[F[_]](val ast: Ast[F]) {
  import ast._
  import Expr._, PredefinedFunction._

  /**
   * Traverse the AST and assign type variables to each expression.
   * No constraint is added at this stage
   */
  def assignVars(vars: TypeVars, expr: Expr): (TypeVars, Expr) =
    expr match {
      case Var(name, _) =>
        vars.withNext(expr.withType)

      case Number(n, _) =>
        vars.withNext(expr.withType)

      case FuncCall(funcName, funcArgs, _) =>
        val (typeVarsWithArgsVars, transformedArgs) =
          vars.traverse(funcArgs)(assignVars)
        typeVarsWithArgsVars.withNext(next => FuncCall(funcName, transformedArgs.reverse, next))

      case Lambda(varName, lambdaBody, _) =>
        val (vars1, typedVar) = assignVars(vars, varName)
        val (vars2, typedBody) = assignVars(vars1, lambdaBody)
        vars2.withNext(next => Lambda(varName.copy(tpe = typedVar.tpe), typedBody, next))
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
      case Number(n, tpe) => (typeVars, Constraints.empty)
      case _              => ???
    }
    val (vars2, constraints2) = vars1.traverse(expr.children)(findConstraints)
    (vars2, constraints1.merge(constraints2))
  }

  def assignVarsAndFindConstraints(expr: Expr): Constraints =
    (findConstraints _).tupled(assignVars(TypeVars.empty, expr))._2

  private def findConstraintsOfPredefinedFunctionCall(typeVars: TypeVars, func: FuncCall): (TypeVars, Constraints) =
    (func.funcId, func.args) match {
      case (Point, x :: y :: Nil) =>
        (typeVars, Constraints(func.tpe -> Type.Point, x.tpe -> Type.Dbl, y.tpe -> Type.Dbl))
      case (Add, x :: y :: Nil) =>
        (typeVars, Constraints(x.tpe -> y.tpe))
      case (Inverse, x :: Nil) =>
        (typeVars, Constraints.empty)
      case (Multiply, x :: y :: Nil) =>
        (typeVars, Constraints(x.tpe -> y.tpe))
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
        (typeVars, Constraints(func.tpe -> Type.Evo(Type.Var("Not used"))))
      case (Cons, x :: y :: Nil) => // TODO I am not sure if we can assume transitivity and remove redundant constraints
        (typeVars, Constraints(func.tpe -> y.tpe, y.tpe -> Type.Evo(x.tpe), func.tpe -> Type.Evo(x.tpe)))
      case (MapEmpty, x :: y :: Nil) =>
        (typeVars, Constraints(func.tpe -> x.tpe, func.tpe -> y.tpe, x.tpe -> y.tpe))
      case (MapCons, x :: f :: Nil) => // TODO we need to generate new type vars...
        (
          typeVars,
          Constraints(
            x.tpe -> Type.Evo(Type.Var("X")),
            func.tpe -> Type.Evo(Type.Var("Y")),
            f.tpe -> Type.Arrow(Type.Var("X"), Type.Arrow(Type.Evo(Type.Var("X")), Type.Evo(Type.Var("Y"))))
          ))

      case (Cartesian, x :: y :: Nil) =>
        (
          typeVars,
          Constraints(func.tpe -> Type.Evo(Type.Point), x.tpe -> Type.Evo(Type.Dbl), y.tpe -> Type.Evo(Type.Dbl)))
      case (Point, x :: y :: Nil) =>
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
        (
          typeVars,
          Constraints(
            x.tpe -> Type.Evo(Type.Var("X")),
            y.tpe -> Type.Evo(Type.Var("X")),
            func.tpe -> Type.Evo(Type.Var("X")),
          ))
      case (Map, x :: f :: Nil) => // TODO we need to generate new type vars
        (
          typeVars,
          Constraints(
            x.tpe -> Type.Evo(Type.Var("X")),
            func.tpe -> Type.Evo(Type.Var("Y")),
            f.tpe -> Type.Arrow(Type.Var("X"), Type.Var("Y"))
          ))
      case (FlatMap, x :: f :: Nil) => // TODO we need to generate new type vars
        (
          typeVars,
          Constraints(
            x.tpe -> Type.Evo(Type.Var("X")),
            func.tpe -> Type.Evo(Type.Var("Y")),
            f.tpe -> Type.Arrow(Type.Var("X"), Type.Evo(Type.Var("Y")))
          ))
      case (Take, n :: e :: Nil) => // TODO we need to generate new type vars
        (
          typeVars,
          Constraints(
            n.tpe -> Type.Integer,
            e.tpe -> Type.Evo(Type.Var("X")),
            func.tpe -> Type.Evo(Type.Var("X")),
          ))

      case (Uniform, from :: to :: Nil) =>
        (typeVars, Constraints(func.tpe -> Type.Evo(Type.Dbl), from.tpe -> Type.Dbl, to.tpe -> Type.Dbl))
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
