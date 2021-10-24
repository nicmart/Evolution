package evolution.compiler.phases.typer

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import evolution.compiler.phases.typer.model.{Assumption, Assumptions, Substitution}
import evolution.compiler.types.Type

private[typer] case class Inference[+T](run: InferenceState => Either[String, (InferenceState, T)]) {
  def runA(is: InferenceState): Either[String, T] = run(is).map(_._2)
}

private[typer] object Inference {
  def newTypeVarname: Inference[String] = Inference(is => Right((is.withNewTypeVar, is.currentTypeVarname)))
  def substitution: Inference[Substitution] = Inference(is => Right((is, is.substitution)))
  def assumptions: Inference[Assumptions] = Inference(is => Right((is, is.assumptions)))
  def setSubstitution(subst: Substitution): Inference[Unit] = Inference(is => Right((is.withSubstitution(subst), ())))
  def setAssumptions(assumptions: Assumptions): Inference[Unit] =
    Inference(is => Right((is.withAssumptions(assumptions), ())))
  def error(message: String): Inference[Nothing] = Inference(_ => Left(message))

  def newTypeVar: Inference[Type] = newTypeVarname.map(Type.Var)

  def withLocalAssumption[T](assumption: Assumption)(ft: Inference[T]): Inference[T] =
    for
      initialAssumptions <- assumptions
      _ <- setAssumptions(initialAssumptions.withAssumption(assumption))
      t <- ft
      _ <- setAssumptions(initialAssumptions)
    yield t

  def getAssumption(name: String): Inference[Assumption] =
    assumptions.map(_.get(name)).flatMap {
      case None             => error(s"assumption not found for varname $name")
      case Some(assumption) => assumption.pure[Inference]
    }

  def fromEither[T](either: Either[String, T]): Inference[T] =
    either.fold(error, InferenceMonad.pure)

  def unify(t1: Type, t2: Type): Inference[Unit] =
    for
      currentSubstitution <- substitution
      unifyingSubstitution <- fromEither(
        Unifier.mostGeneralUnifier(currentSubstitution.substitute(t1), currentSubstitution.substitute(t2))
      )
      _ <- setSubstitution(currentSubstitution.andThen(unifyingSubstitution))
    yield ()

  implicit lazy val inferenceIsMonad: Monad[Inference] = InferenceMonad

  object InferenceMonad extends Monad[Inference] {
    override def pure[A](x: A): Inference[A] = Inference(is => Right((is, x)))
    override def flatMap[A, B](fa: Inference[A])(f: A => Inference[B]): Inference[B] = Inference(
      is =>
        fa.run(is) match {
          case Left(value)    => Left(value)
          case Right((is, a)) => f(a).run(is)
        }
    )
    override def tailRecM[A, B](a: A)(f: A => Inference[Either[A, B]]): Inference[B] =
      Inference(
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
}
