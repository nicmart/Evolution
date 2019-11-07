package evolution.compiler.phases.typer

import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Qualified
import cats.implicits._
import cats.data.State
import evolution.compiler.types.Assumptions
import evolution.compiler.phases.typer.model.TypeVarGenerator
import evolution.compiler.types.Assumption
import evolution.compiler.phases.typer.model.Assignment
import evolution.compiler.phases.typer.model.Substitution
import evolution.compiler.tree._
import evolution.compiler.tree.TreeF._
import evolution.compiler.types.Type.Scheme

private[typer] object AssignFreshTypeVars {

  def assign(expr: Tree, assumptions: Assumptions): TypedTree =
    Tree.catamorphism(assignS)(expr).runA(AssignmentState(TypeVarGenerator.empty, assumptions)).value

  private type S[T] = State[AssignmentState, T]

  private def assignS(tree: TreeF[S[TypedTree]]): S[TypedTree] =
    tree match {
      case Identifier(name, _) =>
        getAssumption(name).flatMap {
          case None =>
            newTypeVar.map(qt => Identifier(name, false).annotate(qt))
          case Some(assumption) =>
            identifier(assumption).widen
        }

      case Lambda(varName, expr) =>
        for {
          varType <- newTypeVar
          bodyWithType <- localAssumption(varName, varType)(expr)
          lambdaType = Qualified(varType.value =>: bodyWithType.annotation.value)
        } yield Lambda(varName, bodyWithType).annotate(lambdaType)

      case App(sf, sarg) =>
        (sf, sarg.sequence, newTypeVar).mapN { (f, arg, qt) =>
          App(f, arg).annotate(qt)
        }

      case Bool(b) =>
        newTypeVar.map(qt => Bool(b).annotate(qt))

      case Let(varName, value, body) =>
        for {
          valueWithType <- value
          bodyWithType <- localAssumption(varName, valueWithType.annotation)(body)
        } yield Let(varName, valueWithType, bodyWithType).annotate(bodyWithType.annotation)

      case IntLiteral(n) =>
        newTypeVar.map(qt => IntLiteral(n).annotate(qt))

      case Lst(sts) =>
        (sts.sequence, newTypeVar).mapN { (ts, qt) =>
          Lst(ts).annotate(Qualified(Type.Lst(qt.value)))
        }

      case DoubleLiteral(n) =>
        newTypeVar.map(qt => DoubleLiteral(n).annotate(qt))
    }

  private case class AssignmentState(vars: TypeVarGenerator, assumptions: Assumptions) {
    def next: AssignmentState = copy(vars = vars.next)
    def withAssumption(name: String, tpe: Qualified[Type]): AssignmentState = copy(
      assumptions = assumptions.withAssumption(Assumption(name, tpe.map(Scheme(_)), false))
    )
  }

  private def newTypeVar: S[Qualified[Type]] = State(s => (s.next, Qualified(s.vars.current)))

  private def localAssumption[T](name: String, tpe: Qualified[Type])(st: S[T]): S[T] =
    for {
      initialState <- State.get
      _ <- withAssumption(name, tpe)
      t <- st
      _ <- resetAssumptions(initialState.assumptions)
    } yield t

  private def withAssumption(name: String, tpe: Qualified[Type]): S[Unit] =
    State.modify[AssignmentState](_.withAssumption(name, tpe))

  private def getAssumption(name: String): S[Option[Assumption]] =
    State.get.map(s => s.assumptions.get(name))

  private def resetAssumptions(assumptions: Assumptions): S[Unit] =
    State.modify[AssignmentState](s => s.copy(assumptions = assumptions))

  // Resolve assumptions for predefined constants schemes
  private def identifier(assumption: Assumption): S[TypedTree] = {
    val typeVars = assumption.qualifiedScheme.value.vars
    for {
      assignments <- typeVars.traverse(
        quantifiedVar => newTypeVar.map(typeVar => Assignment(quantifiedVar, typeVar.value))
      )
      substitution = Substitution(assignments)
      substitutedType = Qualified(
        substitution.substitute(assumption.qualifiedScheme.predicates),
        assumption.qualifiedScheme.value.instantiate(assignments.map(_.tpe))
      )
    } yield Identifier(assumption.name, assumption.primitive).annotate(substitutedType)
  }
}
