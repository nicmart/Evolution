package evolution.compiler.phases.typing

import evolution.compiler.ast.AST
import evolution.compiler.ast.AST.{ App, Identifier, Lambda, Let }
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Qualified
import cats.implicits._
import cats.data.State
import evolution.compiler.types.TypeBindings
import evolution.compiler.phases.typing.model.TypeVarGenerator
import evolution.compiler.types.TypeBinding
import evolution.compiler.phases.typing.model.Assignment
import evolution.compiler.phases.typing.model.Substitution
import evolution.compiler.ast.AST.Lst

object AssignFreshTypeVars {

  def assign(expr: AST, bindings: TypeBindings): AST =
    assignS(expr).runA(AssignmentState(TypeVarGenerator.empty, bindings)).value

  private type S[T] = State[AssignmentState, T]

  /**
   * TODO: can we express this with transformChildren or similar?
   */
  private def assignS(expr: AST): S[AST] =
    expr match {
      // TODO this prevents exhaustive checking
      case _ if expr.tpe.t != Type.Var("") => expr.pure[S]
      case AST.App(f, in, _) =>
        (assignS(f), assignS(in), newTypeVar).mapN { (transformedF, transformedIn, t) =>
          App(transformedF, transformedIn, t)
        }

      // TODO here and in Let we compute constraints. That's because later on it is not possible to find the bindings attached to varName
      case Lambda(varName, body, _) =>
        for {
          varType <- newTypeVar
          bodyWithType <- localBinding(varName.name, varType)(assignS(body))
        } yield Lambda(Identifier(varName.name, varType), bodyWithType, Qualified(varType.t =>: bodyWithType.tpe.t))

      case Let(varName, value, body, _) =>
        for {
          valueWithType <- assignS(value)
          bodyWithType <- localBinding(varName.name, valueWithType.tpe)(assignS(body))
        } yield Let(Identifier(varName.name, valueWithType.tpe), valueWithType, bodyWithType, bodyWithType.tpe)

      case Identifier(name, _, _) =>
        State.get.flatMap { s =>
          s.bindings.getBinding(name) match {
            case None =>
              newTypeVar.map(Identifier(name, _))
            case Some(binding) =>
              identifier(binding).widen
          }
        }

      case Lst(ts, _) =>
        (ts.traverse(assignS), newTypeVar)
          .mapN((assignedTs, newType) => Lst(assignedTs, Qualified(Type.Lst(newType.t))))

      case _ => // No-children expressions. Unsafe, that's why I would like to use transformChildren method
        newTypeVar.map(expr.withType)
    }

  private case class AssignmentState(vars: TypeVarGenerator, bindings: TypeBindings) {
    def next: AssignmentState = copy(vars = vars.next)
    def withBinding(name: String, tpe: Qualified[Type]): AssignmentState = copy(
      bindings = bindings.withVarBinding(name, tpe)
    )
  }

  private def newTypeVar: S[Qualified[Type]] = State(s => (s.next, Qualified(s.vars.current)))

  private def localBinding[T](name: String, tpe: Qualified[Type])(st: S[T]): S[T] =
    for {
      initialState <- State.get
      _ <- withBinding(name, tpe)
      t <- st
      _ <- resetBindings(initialState.bindings)
    } yield t

  private def withBinding(name: String, tpe: Qualified[Type]): S[Unit] =
    State.modify[AssignmentState](_.withBinding(name, tpe))

  private def resetBindings(bindings: TypeBindings): S[Unit] =
    State.modify[AssignmentState](s => s.copy(bindings = bindings))

  private def identifier(binding: TypeBinding): S[Identifier] = {
    binding match {
      case TypeBinding.Variable(_, _) => Identifier(binding.name, binding.qt).pure[S]
      case TypeBinding.Predefined(_, _) =>
        val varsInScheme = binding.qt.t.typeVars.toList
        for {
          assignSments <- varsInScheme.traverse(
            schemeVar => newTypeVar.map(typeVar => Assignment(schemeVar.name, typeVar.t))
          )
          substitution = Substitution(assignSments)
        } yield Identifier(binding.name, substitution.substitute(binding.qt), primitive = true)
    }
  }
}
