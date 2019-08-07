package evolution.compiler.phases.typing

import evolution.compiler.ast.AST
import evolution.compiler.ast.AST.{ App, Identifier, Lambda, Let }
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Qualified
import cats.implicits._
import TypeInference._

object AssignFreshTypeVars {

  /**
   * TODO: can we express this with transformChildren or similar?
   * TODO: can we assign vars directly when we unify?
   * Traverse the AST and assign type variables to each expression.
   * No constraint is added at this stage
   */
  def assign[M[_]](expr: AST)(implicit TI: TypeInference[M]): M[AST] = {

    expr match {
      case _ if expr.tpe.t != Type.Var("") =>
        expr.pure[M]

      case AST.App(f, in, _) =>
        (assign(f), assign(in), newTypeVar).mapN { (transformedF, transformedIn, t) =>
          App(transformedF, transformedIn, t)
        }

      // TODO here and in Let we compute constraints. That's because later on it is not possible to find the bindings attached to varName
      case Lambda(varName, lambdaBody, _) =>
        newTypeVar.flatMap(
          qt =>
            withVarType(varName, qt) {
              assign(lambdaBody).map(b => Lambda(varName, b, Qualified(qt.t =>: b.tpe.t)))
            }
        )

      case Let(varName, value, body, _) =>
        assign(value).flatMap { valueWithVars =>
          withVarType(varName, valueWithVars.tpe) {
            assign(body).map(bodyWithVars => Let(varName, valueWithVars, bodyWithVars, bodyWithVars.tpe))
          }
        }

      case Identifier(name, _, _) =>
        getType(name).map[AST](ast => ast)

      case _ => // No-children expressions. Unsafe, that's why I would like to use transformChildren method
        newTypeVar.map(expr.withType)
    }
  }

}
