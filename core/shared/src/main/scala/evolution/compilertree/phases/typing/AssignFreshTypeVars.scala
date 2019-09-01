package evolution.compilertree.phases.typing

import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Qualified
import cats.implicits._
import cats.data.State
import evolution.compiler.types.TypeBindings
import evolution.compiler.phases.typing.model.TypeVarGenerator
import evolution.compiler.types.TypeBinding
import evolution.compiler.phases.typing.model.Assignment
import evolution.compiler.phases.typing.model.Substitution
import evolution.compilertree.ast.TreeF.Tree
import evolution.compilertree.ast.TreeF
import evolution.compilertree.ast.TreeF._

object AssignFreshTypeVars {

  def assign(expr: Tree, bindings: TypeBindings): CoTree[Qualified[Type]] =
    cata(assignS)(expr).runA(AssignmentState(TypeVarGenerator.empty, bindings)).value

  private type S[T] = State[AssignmentState, T]

  private def assignS(tree: TreeF[S[CoTree[Qualified[Type]]]]): S[CoTree[Qualified[Type]]] =
    tree match {
      case Identifier(name, _) =>
        getBinding(name).flatMap {
          case None =>
            newTypeVar.map(qt => CoTree(qt, Identifier(name, false)))
          case Some(binding) =>
            identifier(binding).widen
        }

      case Lambda(varName, expr) =>
        for {
          varType <- newTypeVar
          bodyWithType <- localBinding(varName, varType)(expr)
        } yield
          CoTree(
            Qualified(varType.value =>: bodyWithType.value.value),
            Lambda(
              varName,
              bodyWithType
            )
          )

      case App(sf, sarg) =>
        (sf, sarg, newTypeVar).mapN { (f, arg, qt) =>
          CoTree(qt, App(f, arg))
        }

      case Bool(b) =>
        newTypeVar.map(qt => CoTree(qt, Bool(b)))

      case Let(varName, value, body) =>
        for {
          valueWithType <- value
          bodyWithType <- localBinding(varName, valueWithType.value)(body)
        } yield
          CoTree(
            bodyWithType.value,
            Let(
              varName,
              valueWithType,
              bodyWithType
            )
          )

      case IntLiteral(n) =>
        newTypeVar.map(qt => CoTree(qt, IntLiteral(n)))

      case Lst(sts) =>
        (sts.sequence, newTypeVar).mapN { (ts, qt) =>
          CoTree(qt, Lst(ts))
        }

      case DoubleLiteral(n) =>
        newTypeVar.map(qt => CoTree(qt, DoubleLiteral(n)))
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

  private def getBinding(name: String): S[Option[TypeBinding]] =
    State.get.map(s => s.bindings.getBinding(name))

  private def resetBindings(bindings: TypeBindings): S[Unit] =
    State.modify[AssignmentState](s => s.copy(bindings = bindings))

  // Resolve type-bindings for predefined constants schemes
  private def identifier(binding: TypeBinding): S[CoTree[Qualified[Type]]] = {
    binding match {
      case TypeBinding.Fixed(_, _) =>
        CoTree(binding.qualifiedType, Identifier(binding.name)).pure[S]
      case TypeBinding.Scheme(_, _) =>
        val varsInScheme = binding.qualifiedType.value.typeVars.toList
        for {
          assignSments <- varsInScheme.traverse(
            schemeVar => newTypeVar.map(typeVar => Assignment(schemeVar.name, typeVar.value))
          )
          substitution = Substitution(assignSments)
        } yield CoTree(substitution.substitute(binding.qualifiedType), Identifier(binding.name, true))
    }
  }
}
