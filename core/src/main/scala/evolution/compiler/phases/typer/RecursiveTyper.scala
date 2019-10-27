package evolution.compiler.phases.typer

//import cats.syntax.either._
import cats.implicits._
import cats.data.State
import evolution.compiler.phases.Typer
import evolution.compiler.module.Module
import evolution.compiler.tree.Tree
import evolution.compiler.types._
import evolution.compiler.types.TypeClasses._
import evolution.compiler.tree._
import model.TypeVarGenerator
import model.Substitution
import evolution.compiler.tree.TreeF.Lst
import evolution.compiler.tree.TreeF.Let
import evolution.compiler.tree.TreeF.Lambda
import evolution.compiler.tree.TreeF.Bool
import evolution.compiler.tree.TreeF.DoubleLiteral
import evolution.compiler.tree.TreeF.IntLiteral
import evolution.compiler.tree.TreeF.Identifier
import RecursiveTyper._
import cats.Monad

final class RecursiveTyper extends Typer {
  def typeTree(tree: Tree, expectedType: Option[Type], module: Module): Either[String, TypedTree] = ???

  def typeTreeF[F[+ _]: Inference](tree: Tree, expectedType: Option[Type], module: Module): F[TypedTree] = {
    val inf: Inference[F] = Inference[F]
    import inf._
    implicit val FMonad: Monad[F] = monad

    tree.value match {
      case Bool(b)          => Bool(b).typeWithNoPredicates(TypeT.Bool).pure[F]
      case DoubleLiteral(n) => DoubleLiteral(n).typeWithNoPredicates(TypeT.Double).pure[F]

      case IntLiteral(n) =>
        for {
          typeVar <- newTypeVar
          predicate = Predicate("Num", List(typeVar))
        } yield IntLiteral(n).typeWithSinglePredicate(predicate, typeVar)

      case Identifier(name, _)    => ???
      case Lst(ts)                => ???
      case Let(varName, expr, in) => ???
      case Lambda(varName, expr)  => ???
      case TreeF.App(f, args)     => ???
    }
  }

  implicit class LeafOps(tree: TreeF[Nothing]) {
    def typed: TreeF[TypedTree] = tree
    def typeWithNoPredicates(tpe: Type): TypedTree = typed.annotate(Qualified(tpe))
    def typeWithSinglePredicate(predicate: Predicate, tpe: Type): TypedTree =
      typed.annotate(Qualified(List(predicate), tpe))
  }
}

sealed trait Inference[F[+ _]] {
  def monad: Monad[F]
  def newTypeVar: F[TypeT.Var]
  def substitution: F[Substitution]
  def typeBindings: F[TypeBindings]
  def setSubstitution(subst: Substitution): F[Unit]
  def setTypeBindings(typeBindings: TypeBindings): F[Unit]
  def error(message: String): F[Nothing]
}

object Inference {
  def apply[F[+ _]](implicit inf: Inference[F]): Inference[F] = inf
}

object RecursiveTyper {
  type S[T] = State[TyperState, T]

  def get: S[TyperState] = State.get
  def set(state: TyperState): S[Unit] = State.set(state)

  def newTypeVar: S[TypeT.Var] = for {
    currentState <- get
    _ <- set(currentState.withNewTypeVar)
  } yield currentState.currentTypeVar

  final class TyperState(private val count: Int, private val subst: Substitution) {
    def currentTypeVar: TypeT.Var = TypeT.Var(s"T$count")
    def withNewTypeVar: TyperState = new TyperState(count + 1, subst)

    def currentSubstitution: Substitution = subst
    def withSubstitution(substitution: Substitution): TyperState = new TyperState(count, subst)
  }

  object TyperState {
    def empty: TyperState = new TyperState(0, Substitution.empty)
  }
}
