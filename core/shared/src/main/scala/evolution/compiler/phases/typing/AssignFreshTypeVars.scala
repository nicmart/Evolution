package evolution.compiler.phases.typing

import evolution.compiler.ast.AST
import evolution.compiler.ast.AST.{ App, Identifier, Lambda, Let }
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Qualified
import cats.implicits._
import evolution.compiler.phases.typing.model.TypeInference
import evolution.compiler.phases.typing.model.TypeInference._
import evolution.compiler.types.TypeBindings

object AssignFreshTypeVars {

  /**
   * TODO: can we express this with transformChildren or similar?
   * TODO: can we assign vars directly when we unify?
   * Traverse the AST and assign type variables to each expression.
   * No constraint is added at this stage
   */
  def assign[M[_]](expr: AST, bindings: TypeBindings)(implicit TI: TypeInference[M]): M[AST] = {

    expr match {
      case _ if expr.tpe.t != Type.Var("") =>
        expr.pure[M]

      case AST.App(f, in, _) =>
        (assign(f, bindings), assign(in, bindings), newTypeVar).mapN { (transformedF, transformedIn, t) =>
          App(transformedF, transformedIn, t)
        }

      // TODO here and in Let we compute constraints. That's because later on it is not possible to find the bindings attached to varName
      case Lambda(varName, body, _) =>
        for {
          varType <- newTypeVar[M]
          bodyWithType <- assign[M](body, bindings.withVarBinding(varName.name, varType))
        } yield Lambda(Identifier(varName.name, varType), bodyWithType, Qualified(varType.t =>: bodyWithType.tpe.t))

      case Let(varName, value, body, _) =>
        for {
          valueWithType <- assign[M](value, bindings)
          bodyWithType <- assign(body, bindings.withVarBinding(varName.name, valueWithType.tpe))
        } yield Let(Identifier(varName.name, valueWithType.tpe), valueWithType, bodyWithType, bodyWithType.tpe)

      case Identifier(name, _, _) =>
        bindings.getIdentifier[M](name).widen

      case _ => // No-children expressions. Unsafe, that's why I would like to use transformChildren method
        newTypeVar.map(expr.withType)
    }
  }
}
