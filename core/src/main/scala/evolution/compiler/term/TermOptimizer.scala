package evolution.compiler.term

import evolution.compiler.phases.typer.config.{ConstConfig, TypingConfig}
import evolution.compiler.term.Term.Literal.LitList
import evolution.compiler.term.Term._

import scala.util.Try

final class TermOptimizer(interpreter: TermInterpreter) {
  def optimize(term: Term): Term = {
    val termWithOptimizedChildren = term match {
      case Lit(LitList(ts))                    => Lit(LitList(ts.map(optimize)))
      case Lit(_) | Inst(_) | Value(_) | Id(_) => term
      case Apply(f, x)                         => Apply(optimize(f), optimize(x))
      case Let(name, body, in) =>
        optimize(body) match {
          case Value(body)   => optimize(replaceId(name, in, Value(body))) // inlining
          case optimizedBody => Let(name, optimizedBody, optimize(in))
        }
      case Lambda(name, body) => Lambda(name, optimize(body))
    }

    val optimized =
      if (freeVars(termWithOptimizedChildren).nonEmpty) termWithOptimizedChildren
      else Value(interpreter.interpret(termWithOptimizedChildren))

    if (optimized == term) optimized else optimize(optimized)
  }

  private val consts: Set[String] = ConstConfig.constants.map(_.name).toSet

  private def freeVars(term: Term): Set[String] = freeVarsWithConsts(term).diff(consts)

  private def freeVarsWithConsts(term: Term): Set[String] = term match {
    case Lit(LitList(ts))      => ts.flatMap(freeVarsWithConsts).toSet
    case Lit(_)                => Set.empty
    case Id(name)              => Set(name)
    case Inst(_)               => Set.empty
    case Let(name, expr, body) => freeVarsWithConsts(expr) ++ freeVarsWithConsts(body).diff(Set(name))
    case Lambda(name, body)    => freeVarsWithConsts(body).diff(Set(name))
    case Apply(f, x)           => freeVarsWithConsts(f) ++ freeVarsWithConsts(x)
    case Value(_)              => Set.empty
  }

  private def replaceId(name: String, term: Term, replaceWith: Term): Term =
    term match {
      case Lit(LitList(ts)) => Lit(LitList(ts.map(replaceId(name, _, replaceWith))))
      case Id(id)           => if (id == name) replaceWith else term
      case Let(id, expr, body) =>
        if (id == name) term else Let(id, replaceId(name, expr, replaceWith), replaceId(name, body, replaceWith))
      case Lambda(id, body) => if (id == name) term else Lambda(id, replaceId(name, body, replaceWith))
      case Apply(f, x)      => Apply(replaceId(name, f, replaceWith), replaceId(name, x, replaceWith))
      case _                => term
    }
}
