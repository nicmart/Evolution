package evolution.compiler.term

import cats.data.Reader
import cats.implicits._
import evolution.compiler.phases.typer.config.ConstConfig
import evolution.compiler.term.Term.Literal._
import evolution.compiler.term.Term.{Id, _}
import evolution.compiler.term.TermOptimizer._

final class TermOptimizer(interpreter: TermInterpreter) {
  def optimize(term: Term): Term =
    optimizeM(term)
      .run(Env.consts)
      .term

  private def optimizeM(term: Term): Optimized[OptimizedTerm] = {
    term match {
      case Lit(LitList(ts)) =>
        for {
          optimizedTs <- ts.traverse(optimizeM)
          freeVars = optimizedTs.flatMap(_.freeVars).toSet
          valueTerms = optimizedTs.map(_.term).collect { case Value(t) => t }
          newTerm = if (valueTerms.length == ts.length) Value(valueTerms) else term
        } yield OptimizedTerm(newTerm, freeVars)

      case Lit(_) | Inst(_) | Value(_) =>
        OptimizedTerm(Value(interpreter.interpret(term)), Set.empty).pure[Optimized]

      case Id(name) =>
        for {
          maybeBoundTerm <- binding(name)
          idTerm = maybeBoundTerm.getOrElse(term)
        } yield OptimizedTerm(inline(name, idTerm), Set(name))

      case Apply(f, x) =>
        for {
          f <- optimizeM(f)
          x <- optimizeM(x)
          optApply <- optimizeApplyLambda(Apply(f.term, x.term))
          optValues <- optimizeApplyValues(optApply.term)
        } yield OptimizedTerm(optValues.term, f.freeVars ++ x.freeVars)

      case Let(name, body, in) =>
        for {
          body <- optimizeM(body)
          in <- bindLocal(name, body.term)(optimizeM(in))
          optLet = optimizeLet(Let(name, body.term, in.term))
        } yield OptimizedTerm(optLet, body.freeVars ++ (in.freeVars - name))

      case Lambda(name, body) =>
        for {
          body <- unbindLocal(name)(optimizeM(body))
          freeVars = body.freeVars.diff(Set(name))
        } yield OptimizedTerm(Lambda(name, body.term), freeVars)
    }
  }

  private def optimizeLet(term: Let): Term =
    term.body match {
      case Value(v) => Value(v)
      case _        => term
    }

  private def optimizeApplyLambda(term: Term): Optimized[OptimizedTerm] =
    term match {
      case Apply(Lambda(name, body), x) =>
        for {
          x <- optimizeM(x)
          body <- bindLocal(name, x.term)(optimizeM(body))
        } yield body
      case _ => OptimizedTerm(term, Set.empty).pure[Optimized]
    }

  private def optimizeApplyValues(term: Term): Optimized[OptimizedTerm] =
    term match {
      case Apply(Value(f), Value(x)) =>
        OptimizedTerm(Value(f.asInstanceOf[Any => Any](x)), Set.empty).pure[Optimized]
      case _ =>
        OptimizedTerm(term, Set.empty).pure[Optimized]
    }

  private def inline(id: String, term: Term): Term =
    term match {
      case Value(_) => term
      case _        => term
    }
}

object TermOptimizer {
  final case class Env(bindings: Map[String, Term]) {
    def bind(name: String, term: Term): Env = Env(bindings.updated(name, term))
    def unbind(name: String): Env = Env(bindings.removed(name))
  }

  object Env {
    val consts = Env(Map(ConstConfig.constants.map(c => c.name -> Value(c.value)): _*))
  }

  final case class OptimizedTerm(term: Term, freeVars: Set[String], closingVars: Set[String] = Set.empty) {
    def freeNonConstantVars: Set[String] = freeVars.diff(ConstConfig.constants.map(_.name).toSet)
  }

  def bindLocal[T](name: String, term: Term)(ft: => Optimized[T]): Optimized[T] =
    Reader.local[T, Env](_.bind(name, term))(ft)

  def unbindLocal[T](name: String)(ft: => Optimized[T]): Optimized[T] =
    Reader.local[T, Env](_.unbind(name))(ft)

  def binding(name: String): Optimized[Option[Term]] =
    Reader(_.bindings.get(name))

  val envVars: Optimized[Set[String]] = Reader(_.bindings.keySet)

  type Optimized[T] = Reader[Env, T]
}
