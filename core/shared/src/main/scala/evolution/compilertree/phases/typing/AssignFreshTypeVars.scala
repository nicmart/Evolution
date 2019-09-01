package evolution.compilertree.phases.typing

import evolution.compilertree.types.Type
import evolution.compilertree.types.TypeClasses.Qualified
import cats.implicits._
import cats.data.State
import evolution.compilertree.types.TypeBindings
import evolution.compilertree.phases.typing.model.TypeVarGenerator
import evolution.compilertree.types.TypeBinding
import evolution.compilertree.phases.typing.model.Assignment
import evolution.compilertree.phases.typing.model.Substitution
import evolution.compilertree.ast.TreeF.Tree
import evolution.compilertree.ast.TreeF
import evolution.compilertree.ast.TreeF._

object AssignFreshTypeVars {

  def assign(expr: Tree, bindings: TypeBindings): TypedTree =
    cata(assignS)(expr).runA(AssignmentState(TypeVarGenerator.empty, bindings)).value

  private type S[T] = State[AssignmentState, T]

  private def assignS(tree: TreeF[S[TypedTree]]): S[TypedTree] =
    tree match {
      case Identifier(name, _) =>
        getBinding(name).flatMap {
          case None =>
            newTypeVar.map(qt => Identifier(name, false).annotate(qt))
          case Some(binding) =>
            identifier(binding).widen
        }

      case Lambda(varName, expr) =>
        for {
          varType <- newTypeVar
          bodyWithType <- localBinding(varName, varType)(expr)
          lambdaType = Qualified(varType.value =>: bodyWithType.value.value)
        } yield Lambda(varName, bodyWithType).annotate(lambdaType)

      case App(sf, sarg) =>
        (sf, sarg, newTypeVar).mapN { (f, arg, qt) =>
          App(f, arg).annotate(qt)
        }

      case Bool(b) =>
        newTypeVar.map(qt => Bool(b).annotate(qt))

      case Let(varName, value, body) =>
        for {
          valueWithType <- value
          bodyWithType <- localBinding(varName, valueWithType.value)(body)
        } yield Let(varName, valueWithType, bodyWithType).annotate(bodyWithType.value)

      case IntLiteral(n) =>
        newTypeVar.map(qt => IntLiteral(n).annotate(qt))

      case Lst(sts) =>
        (sts.sequence, newTypeVar).mapN { (ts, qt) =>
          Lst(ts).annotate(Qualified(Type.Lst(qt.value)))
        }

      case DoubleLiteral(n) =>
        newTypeVar.map(qt => DoubleLiteral(n).annotate(qt))
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
  private def identifier(binding: TypeBinding): S[TypedTree] = {
    binding match {
      case TypeBinding.Fixed(_, _) =>
        Identifier(binding.name).annotate(binding.qualifiedType).pure[S]
      case TypeBinding.Scheme(_, _) =>
        val varsInScheme = binding.qualifiedType.value.typeVars.toList
        for {
          assignSments <- varsInScheme.traverse(
            schemeVar => newTypeVar.map(typeVar => Assignment(schemeVar.name, typeVar.value))
          )
          substitution = Substitution(assignSments)
          substitutedType = substitution.substitute(binding.qualifiedType)
        } yield Identifier(binding.name, true).annotate(substitutedType)
    }
  }
}
