package evolution.compiler.term

import cats.data.Reader
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.traverse._
import evolution.compiler.phases.typer.config.ConstConfig
import evolution.compiler.term.Term.Literal._
import evolution.compiler.term.Term.{Id, _}
import evolution.compiler.term.TermOptimizer._

import scala.annotation.tailrec

final class TermOptimizer(interpreter: TermInterpreter) {
  private val renamer = new CaptureAvoidingRenamer
  def optimize(term: Term, definitions: Map[String, Term]): Term = {
    val renamedTerm = renamer.rename(term) // Is this needed?
    optimizeM(renamedTerm).run(Env(definitions))
  }

  private def optimizeM(term: Term): Optimized[Term] = {
    term match {
      case Lit(LitList(ts)) =>
        for {
          optimizedTs <- ts.traverse(optimizeM)
          valueTerms = optimizedTs.collect { case Value(t) => t }
          newTerm = if (valueTerms.length == ts.length) Value(valueTerms) else Lit(LitList(optimizedTs))
        } yield newTerm

      case Lit(_) | Inst(_) | Value(_) =>
        Value(interpreter.interpret(term)).pure[Optimized].widen

      case Id(name) =>
        for {
          maybeBoundTerm <- binding(name)
          idTerm = maybeBoundTerm.getOrElse(term)
        } yield this.inline(name, idTerm)

      case Apply(f, x) =>
        for {
          f <- optimizeM(f)
          x <- optimizeM(x)
          optApply <- optimizeApplyLambda(f, x)
          optValues <- optimizeApplyValues(optApply)
        } yield optValues

      case Let(name, body, in) =>
        for {
          body <- optimizeM(body)
          in <- bindLocal(name, body)(optimizeM(in))
          optLet = optimizeLet(Let(name, body, in))
        } yield optLet

      case lambda @ Lambda(name, _) =>
        for {
          isNameAlreadyInUse <- hasBinding(name)
          optimized <- if (isNameAlreadyInUse) optimizeM(renamer.rename(term, Set(name))) else optimizeLambda(lambda)
        } yield optimized
    }
  }

  private def optimizeLambda(lambda: Term.Lambda): Optimized[Term] =
    for {
      body <- bindLocal(lambda.name, Id(lambda.name))(optimizeM(lambda.body))
    } yield Lambda(lambda.name, body)

  private def optimizeLet(term: Let): Term =
    term.body match {
      case Value(v) => Value(v)
      case _        => term
    }

  private def optimizeApplyLambda(f: Term, x: Term): Optimized[Term] =
    f match {
      case Lambda(name, body) => // Do we need to do some renaming here as well?
        for {
          body <- bindLocal(name, x)(optimizeM(body))
        } yield body
      case _ => Apply(f, x).pure[Optimized].widen
    }

  private def optimizeApplyValues(term: Term): Optimized[Term] =
    term match {
      case Apply(Value(f), Value(x)) =>
        Value(f.asInstanceOf[Any => Any](x)).pure[Optimized].widen
      case _ =>
        term.pure[Optimized]
    }

  private def inline(id: String, term: Term): Term =
    term match {
      case Value(_) => term
      // Without this inlining some drawings, like oscillator drawn on brownians are very slow
      case _ => term
    }
}

object TermOptimizer {
  final case class Env(bindings: Map[String, Term]) {
    def bind(name: String, term: Term): Env =
      Env(bindings.updated(name, term))

    @tailrec
    def freshName(name: String): String =
      if (bindings.isDefinedAt(name)) freshName(name + "'")
      else name

    def unbind(name: String): Env = Env(bindings.removed(name))
  }

  object Env {
    val consts: Env = Env(Map(ConstConfig.constants.map(c => c.name -> Value(c.value)): _*))
  }

  def bindLocal[T](name: String, term: Term)(ft: => Optimized[T]): Optimized[T] =
    Reader.local[T, Env](_.bind(name, term))(ft)

  def unbindLocal[T](name: String)(ft: => Optimized[T]): Optimized[T] =
    Reader.local[T, Env](_.unbind(name))(ft)

  def binding(name: String): Optimized[Option[Term]] =
    Reader(_.bindings.get(name))

  def freshName(name: String): Optimized[String] =
    Reader(_.freshName(name))

  def hasBinding(name: String): Optimized[Boolean] =
    Reader(_.bindings.isDefinedAt(name))

  val envVars: Optimized[Set[String]] = Reader(_.bindings.keySet)

  type Optimized[T] = Reader[Env, T]
}
