package evolution.compiler.phases.typer

//import cats.syntax.either._
import cats.{ Functor, Monad }
import cats.implicits._
import evolution.compiler.module.Module
import evolution.compiler.phases.Typer
import evolution.compiler.phases.typer.RecursiveTyper.ReprInference._
import evolution.compiler.phases.typer.RecursiveTyper._
import evolution.compiler.phases.typer.model.{ Assignment, Substitution }
import evolution.compiler.tree.TreeF.{ Bool, DoubleLiteral, Identifier, IntLiteral, Lambda, Let, Lst }
import evolution.compiler.tree.{ Tree, _ }
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses._
import evolution.compiler.types._

final class RecursiveTyper extends Typer {

  def typeTree(tree: Tree, expectedType: Option[Type], module: Module): Either[String, TypedTree] =
    typeTreeF(tree, expectedType, module).runA(InferenceState.empty)

  private[typer] def typeTreeF(tree: Tree, expectedType: Option[Type], module: Module): Repr[TypedTree] = {
    tree.value match {
      case Bool(b) => Bool(b).typeWithNoPredicates(Type.Bool).pure[Repr]

      case DoubleLiteral(n) => DoubleLiteral(n).typeWithNoPredicates(Type.Double).pure[Repr]

      case IntLiteral(n) =>
        for {
          typeVar <- newTypeVar
          predicate = Predicate("Num", List(typeVar))
        } yield IntLiteral(n).typeWithSinglePredicate(predicate, typeVar)

      case Identifier(name, primitive) =>
        for {
          assumption <- getAssumption(name)
          qualifiedScheme = assumption.qualifiedScheme
          vars = qualifiedScheme.value.vars.map(Type.Var)
          freshTypeVars <- vars.traverse(_ => newTypeVar)
          newQualifiedType = instantiate(qualifiedScheme, freshTypeVars)
        } yield Identifier(name, primitive).annotate(newQualifiedType)

      case Lambda(varName, body) =>
        for {
          freshTypeVarname <- newTypeVarname
          assumption = Assumption(varName, Qualified(Scheme(Type.Var(freshTypeVarname))), false)
          typedBody <- withLocalAssumption(assumption)(typeTreeF(body, None, module))
          lambdaType = Type.Var(freshTypeVarname) =>: typedBody.annotation.value
          lambdaPredicates = typedBody.annotation.predicates
          lambdaQualifiedType = Qualified(lambdaPredicates, lambdaType)
        } yield Lambda(varName, typedBody).annotate(lambdaQualifiedType)

      case Lst(ts)                => ???
      case Let(varName, expr, in) => ???
      case TreeF.App(f, args)     => ???
    }
  }
}

object RecursiveTyper {
  def instantiate(qs: Qualified[Scheme], types: List[Type]): Qualified[Type] = {
    val assignments = qs.value.vars.zip(types).map { case (from, to) => Assignment(from, to) }
    val substitution = Substitution(assignments)
    Qualified(substitution.substitute(qs.predicates), qs.value.instantiate(types))
  }

  sealed trait Inference[F[+ _]] extends InferenceOps[F] {
    def newTypeVarname: F[String]
    def substitution: F[Substitution]
    def assumptions: F[Assumptions]
    def setSubstitution(subst: Substitution): F[Unit]
    def setAssumptions(assumptions: Assumptions): F[Unit]
    def error(message: String): F[Nothing]
  }

  trait InferenceOps[F[+ _]] { self: Inference[F] =>
    final def newTypeVar(implicit M: Functor[F]): F[Type] = newTypeVarname.map(Type.Var)

    final def withLocalAssumption[T](assumption: Assumption)(ft: F[T])(implicit M: Monad[F]): F[T] =
      for {
        initialAssumptions <- assumptions
        _ <- setAssumptions(initialAssumptions.withAssumption(assumption))
        t <- ft
        _ <- setAssumptions(initialAssumptions)
      } yield t

    final def getAssumption(name: String)(implicit M: Monad[F]): F[Assumption] =
      assumptions.map(_.get(name)).flatMap {
        case None             => error(s"assumption not found for varname $name")
        case Some(assumption) => assumption.pure[F]
      }
  }

  implicit class LeafOps(tree: TreeF[Nothing]) {
    def typed: TreeF[TypedTree] = tree
    def typeWithNoPredicates(tpe: Type): TypedTree = typed.annotate(Qualified(tpe))
    def typeWithSinglePredicate(predicate: Predicate, tpe: Type): TypedTree =
      typed.annotate(Qualified(List(predicate), tpe))
  }

  case class Repr[+T](run: InferenceState => Either[String, (InferenceState, T)]) {
    def runA(is: InferenceState): Either[String, T] = run(is).map(_._2)
  }

  object ReprInference extends Inference[Repr] {
    override def newTypeVarname: Repr[String] = Repr(is => Right((is.withNewTypeVar, is.currentTypeVarname)))
    override def substitution: Repr[Substitution] = Repr(is => Right((is, is.substitution)))
    override def assumptions: Repr[Assumptions] = Repr(is => Right((is, is.assumptions)))
    override def setSubstitution(subst: Substitution): Repr[Unit] = Repr(is => Right((is.withSubstitution(subst), ())))
    override def setAssumptions(assumptions: Assumptions): Repr[Unit] =
      Repr(is => Right((is.withAssumptions(assumptions), ())))
    override def error(message: String): Repr[Nothing] = Repr(_ => Left(message))
  }

  implicit def reprMonadInstance: Monad[Repr] = ReprMonad

  object ReprMonad extends Monad[Repr] {
    override def pure[A](x: A): Repr[A] = Repr(is => Right((is, x)))
    override def flatMap[A, B](fa: Repr[A])(f: A => Repr[B]): Repr[B] = Repr(
      is =>
        fa.run(is) match {
          case Left(value)    => Left(value)
          case Right((is, a)) => f(a).run(is)
        }
    )
    override def tailRecM[A, B](a: A)(f: A => Repr[Either[A, B]]): Repr[B] =
      Repr(
        is =>
          f(a).run(is) match {
            case Left(value) => Left(value)
            case Right((is2, aOrb)) =>
              aOrb match {
                case Left(a)  => tailRecM(a)(f).run(is2)
                case Right(b) => Right((is2, b))
              }
          }
      )
  }

  final case class InferenceState(
    private val count: Int,
    substitution: Substitution,
    assumptions: Assumptions
  ) {
    def currentTypeVarname: String = s"T$count"
    def currentTypeVar: Type = Type.Var(currentTypeVarname)
    def withNewTypeVar: InferenceState = copy(count = count + 1)

    def withSubstitution(substitution: Substitution): InferenceState =
      copy(substitution = substitution)

    def withAssumptions(assumptions: Assumptions): InferenceState =
      copy(assumptions = assumptions)
  }

  object InferenceState {
    def empty: InferenceState = new InferenceState(0, Substitution.empty, Assumptions.empty)
  }
}
