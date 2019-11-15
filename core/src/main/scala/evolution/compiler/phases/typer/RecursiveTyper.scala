package evolution.compiler.phases.typer

//import cats.syntax.either._
import cats.implicits._
import cats.{Functor, Monad}
import evolution.compiler.phases.Typer
import evolution.compiler.phases.typer.RecursiveTyper.ReprInference._
import evolution.compiler.phases.typer.RecursiveTyper._
import evolution.compiler.phases.typer.model.{Assignment, Substitution}
import evolution.compiler.tree.TreeF.{Bool, DoubleLiteral, Id, IntLiteral, Lambda, Let, Lst}
import evolution.compiler.tree.{Tree, _}
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses._
import evolution.compiler.types._

final class RecursiveTyper extends Typer {

  def typeTree(
      tree: Tree,
      expectedType: Option[Type],
      assumptions: Assumptions
  ): Either[String, TypedTree] =
    typeTreeAndSubstitute(tree, expectedType, assumptions).runA(
      InferenceState.empty
    )

  private def typeTreeAndSubstitute(
      tree: Tree,
      expectedType: Option[Type],
      initialAssumptions: Assumptions
  ): Repr[TypedTree] =
    for {
      currentAssumptions <- assumptions
      _ <- setAssumptions(currentAssumptions.merge(initialAssumptions))
      typed <- typeTreeF(tree)
      _ <- expectedType.fold(ReprMonad.pure(()))(expected => unify(expected, typed.annotation.value))
      finalSubstitution <- substitution
    } yield finalSubstitution.substitute(typed)

  private def typeTreeF(tree: Tree): Repr[TypedTree] = {
    tree.value match {
      case Bool(b) => Bool(b).typeWithNoPredicates(Type.Bool).pure[Repr]

      case DoubleLiteral(n) =>
        DoubleLiteral(n).typeWithNoPredicates(Type.Double).pure[Repr]

      case IntLiteral(n) =>
        for {
          typeVar <- newTypeVar
          predicate = Predicate("Num", List(typeVar))
        } yield IntLiteral(n).typeWithSinglePredicate(predicate, typeVar)

      case Id(name, primitive) =>
        for {
          assumption <- getAssumption(name)
          qualifiedScheme = assumption.qualifiedScheme
          vars = qualifiedScheme.value.vars.map(Type.Var)
          freshTypeVars <- vars.traverse(_ => newTypeVar)
          newQualifiedType = instantiate(qualifiedScheme, freshTypeVars)
        } yield Id(name, assumption.primitive).annotate(newQualifiedType)

      case Lambda(varName, body) =>
        for {
          freshTypeVarname <- newTypeVarname
          assumption = Assumption(varName, Qualified(Scheme(Type.Var(freshTypeVarname))), false)
          typedBody <- withLocalAssumption(assumption)(typeTreeF(body))
          lambdaType = Type.Var(freshTypeVarname) =>: typedBody.annotation.value
          lambdaPredicates = typedBody.annotation.predicates
          lambdaQualifiedType = qualified(lambdaPredicates, lambdaType)
        } yield Lambda(varName, typedBody).annotate(lambdaQualifiedType)

      case TreeF.App(f, inputs) =>
        for {
          typedF <- typeTreeF(f)
          typedInputs <- inputs.traverse(tree => typeTreeF(tree))
          inputTypes = typedInputs.map(_.annotation.value).toList
          inputPredicates = typedInputs.toList.flatMap(_.annotation.predicates)
          returnType <- newTypeVar
          _ <- unify(typedF.annotation.value, arrowType(inputTypes, returnType))
          qualifiedType = qualified(typedF.annotation.predicates ++ inputPredicates, returnType)
        } yield TreeF.App(typedF, typedInputs).annotate(qualifiedType)

      case Lst(ts) =>
        for {
          typedTs <- ts.traverse(tree => typeTreeF(tree))
          freshTypeVar <- newTypeVar
          _ <- typedTs.traverse(typedTree => unify(freshTypeVar, typedTree.annotation.value))
          qualifiedType = qualified(typedTs.flatMap(_.annotation.predicates), Type.Lst(freshTypeVar))
        } yield Lst(typedTs).annotate(qualifiedType)

      case Let(varName, expr, in) =>
        for {
          typedExpr <- typeTreeF(expr)
          assumption = Assumption(varName, typedExpr.annotation.map(Scheme.apply), false)
          typedIn <- withLocalAssumption(assumption)(typeTreeF(in))
          letPredicates = typedExpr.annotation.predicates ++ typedIn.annotation.predicates
          letType = qualified(letPredicates, typedIn.annotation.value)
        } yield Let(varName, typedExpr, typedIn).annotate(letType)
    }
  }
}

object RecursiveTyper {
  // TODO move outside
  private def mostGeneralUnifier(a: Type, b: Type): Either[String, Substitution] = (a, b) match {
    case (Type.Arrow(a1, a2), Type.Arrow(b1, b2)) =>
      for {
        s1 <- mostGeneralUnifier(a1, b1)
        s2 <- mostGeneralUnifier(s1.substitute(a2), s1.substitute(b2))
      } yield s1.andThen(s2)
    case (Type.Var(x), t)           => varBind(x, t)
    case (t, Type.Var(x))           => varBind(x, t)
    case (Type.Lst(a), Type.Lst(b)) => mostGeneralUnifier(a, b)
    case (Type.Evo(a), Type.Evo(b)) => mostGeneralUnifier(a, b)
    case _ if a == b                => Right(Substitution.empty)
    case _                          => Left(s"$a can't be unified with $b")
  }

  private def varBind(name: String, tpe: Type): Either[String, Substitution] =
    if (tpe == Type.Var(name)) Right(Substitution.empty)
    else if (tpe.typeVarUsages(name).isEmpty) Right(Substitution(name -> tpe))
    else Left(s"$name cannot be bound to $tpe")

  private def qualified(predicates: List[Predicate], tpe: Type): Qualified[Type] =
    Qualified(predicates.distinct, tpe)

  private def instantiate(qs: Qualified[Scheme], types: List[Type]): Qualified[Type] = {
    val assignments =
      qs.value.vars.zip(types).map { case (from, to) => Assignment(from, to) }
    val substitution = Substitution(assignments)
    Qualified(
      substitution.substitute(qs.predicates),
      qs.value.instantiate(types)
    )
  }

  def arrowType(inputs: List[Type], result: Type): Type =
    inputs.foldRight(result)(_ =>: _)

  sealed trait Inference[F[+_]] extends InferenceOps[F] {
    def newTypeVarname: F[String]
    def substitution: F[Substitution]
    def assumptions: F[Assumptions]
    def setSubstitution(subst: Substitution): F[Unit]
    def setAssumptions(assumptions: Assumptions): F[Unit]
    def error(message: String): F[Nothing]
  }

  trait InferenceOps[F[+_]] { self: Inference[F] =>
    final def newTypeVar(implicit M: Functor[F]): F[Type] =
      newTypeVarname.map(Type.Var)

    final def withLocalAssumption[T](
        assumption: Assumption
    )(ft: F[T])(implicit M: Monad[F]): F[T] =
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

    final def fromEither[T](
        either: Either[String, T]
    )(implicit M: Monad[F]): F[T] =
      either.fold(error, M.pure)

    final def unify(t1: Type, t2: Type)(implicit M: Monad[F]): F[Unit] =
      for {
        currentSubstitution <- substitution
        unifyingSubstitution <- fromEither(
          mostGeneralUnifier(currentSubstitution.substitute(t1), currentSubstitution.substitute(t2))
        )
        _ <- setSubstitution(currentSubstitution.andThen(unifyingSubstitution))
      } yield ()
  }

  implicit class LeafOps(tree: TreeF[Nothing]) {
    def typed: TreeF[TypedTree] = tree
    def typeWithNoPredicates(tpe: Type): TypedTree =
      typed.annotate(Qualified(tpe))
    def typeWithSinglePredicate(predicate: Predicate, tpe: Type): TypedTree =
      typed.annotate(Qualified(List(predicate), tpe))
  }

  case class Repr[+T](
      run: InferenceState => Either[String, (InferenceState, T)]
  ) {
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

  implicit val reprMonadInstance: Monad[Repr] = ReprMonad

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
    def empty: InferenceState =
      new InferenceState(0, Substitution.empty, Assumptions.empty)
  }
}
