package evolution.compilertree.phases.typing

import evolution.compilertree.ast.AST
import evolution.compilertree.ast.AST.{ App, Identifier, Lambda, Let }
import evolution.compilertree.types.Type
import evolution.compilertree.types.TypeClasses.Qualified
import cats.implicits._
import cats.data.State
import evolution.compilertree.types.TypeBindings
import evolution.compilertree.phases.typing.model.TypeVarGenerator
import evolution.compilertree.types.TypeBinding
import evolution.compilertree.phases.typing.model.Assignment
import evolution.compilertree.phases.typing.model.Substitution
import evolution.compilertree.ast.AST.Lst
import evolution.compilertree.ast.TreeF.Tree
import evolution.compilertree.ast.TreeF
import evolution.compilertree.ast.TreeF._

object AssignFreshTypeVarsTree {

  def assign(expr: Tree, bindings: TypeBindings): CoTree[Qualified[Type]] =
    cata(doTheJob)(expr).runA(AssignmentState(TypeVarGenerator.empty, bindings)).value

  private type S[T] = State[AssignmentState, T]

  private def doTheJob(tree: TreeF[S[CoTree[Qualified[Type]]]]): S[CoTree[Qualified[Type]]] =
    tree match {
      case TreeF.Identifier(name, _) =>
        getBinding(name).flatMap {
          case None =>
            newTypeVar.map(qt => CoTree(qt, TreeF.Identifier(name, false)))
          case Some(binding) =>
            identifier(binding).widen
        }

      case TreeF.Lambda(varName, expr) =>
        for {
          varType <- newTypeVar
          bodyWithType <- localBinding(varName, varType)(expr)
        } yield
          CoTree(
            Qualified(varType.value =>: bodyWithType.value.value),
            TreeF.Lambda(
              varName,
              bodyWithType
            )
          )

      case TreeF.App(sf, sarg) =>
        (sf, sarg, newTypeVar).mapN { (f, arg, qt) =>
          CoTree(qt, TreeF.App(f, arg))
        }

      case TreeF.Bool(b) =>
        newTypeVar.map(qt => CoTree(qt, TreeF.Bool(b)))

      case TreeF.Let(varName, value, body) =>
        for {
          valueWithType <- value
          bodyWithType <- localBinding(varName, valueWithType.value)(body)
        } yield
          CoTree(
            bodyWithType.value,
            TreeF.Let(
              varName,
              valueWithType,
              bodyWithType
            )
          )

      case TreeF.IntLiteral(n) =>
        newTypeVar.map(qt => CoTree(qt, TreeF.IntLiteral(n)))

      case TreeF.Lst(sts) =>
        (sts.sequence, newTypeVar).mapN { (ts, qt) =>
          CoTree(
            qt,
            TreeF.Lst(ts)
          )
        }

      case TreeF.DoubleLiteral(n) =>
        newTypeVar.map(qt => CoTree(qt, TreeF.DoubleLiteral(n)))
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
        CoTree(binding.qualifiedType, TreeF.Identifier(binding.name)).pure[S]
      case TypeBinding.Scheme(_, _) =>
        val varsInScheme = binding.qualifiedType.value.typeVars.toList
        for {
          assignSments <- varsInScheme.traverse(
            schemeVar => newTypeVar.map(typeVar => Assignment(schemeVar.name, typeVar.value))
          )
          substitution = Substitution(assignSments)
        } yield CoTree(substitution.substitute(binding.qualifiedType), TreeF.Identifier(binding.name, true))
    }
  }
}

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
      case _ if expr.qualifiedType.value != Type.Var("") => expr.pure[S]
      case AST.App(f, in, _) =>
        (assignS(f), assignS(in), newTypeVar).mapN { (transformedF, transformedIn, t) =>
          App(transformedF, transformedIn, t)
        }

      // TODO here and in Let we compute constraints. That's because later on it is not possible to find the bindings attached to varName
      case Lambda(varName, body, _) =>
        for {
          varType <- newTypeVar
          bodyWithType <- localBinding(varName.name, varType)(assignS(body))
        } yield
          Lambda(
            Identifier(varName.name, varType),
            bodyWithType,
            Qualified(varType.value =>: bodyWithType.qualifiedType.value)
          )

      case Let(varName, value, body, _) =>
        for {
          valueWithType <- assignS(value)
          bodyWithType <- localBinding(varName.name, valueWithType.qualifiedType)(assignS(body))
        } yield
          Let(
            Identifier(varName.name, valueWithType.qualifiedType),
            valueWithType,
            bodyWithType,
            bodyWithType.qualifiedType
          )

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
          .mapN((assignedTs, newType) => Lst(assignedTs, Qualified(Type.Lst(newType.value))))

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

  // Resolve type-bindings for predefined constants schemes
  private def identifier(binding: TypeBinding): S[Identifier] = {
    binding match {
      case TypeBinding.Fixed(_, _) => Identifier(binding.name, binding.qualifiedType).pure[S]
      case TypeBinding.Scheme(_, _) =>
        val varsInScheme = binding.qualifiedType.value.typeVars.toList
        for {
          assignSments <- varsInScheme.traverse(
            schemeVar => newTypeVar.map(typeVar => Assignment(schemeVar.name, typeVar.value))
          )
          substitution = Substitution(assignSments)
        } yield Identifier(binding.name, substitution.substitute(binding.qualifiedType), primitive = true)
    }
  }
}
