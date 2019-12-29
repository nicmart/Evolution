package evolution.compiler.term

import Term._
import evolution.compiler.term.Term.Literal.LitList

object TermRenamer {
  def rename(oldName: String, newName: String)(term: Term): Term =
    term match {
      case Id(name) =>
        Id(if (name == oldName) newName else name)
      case Let(name, expr, body) =>
        Let(name, rename(oldName, newName)(expr), if (name == oldName) body else rename(oldName, newName)(body))
      case Lambda(name, body) =>
        Lambda(name, if (name == oldName) body else rename(oldName, newName)(body))
      case Apply(f, x) =>
        Apply(rename(oldName, newName)(f), rename(oldName, newName)(x))
      case Lit(LitList(ts)) =>
        Lit(LitList(ts.map(rename(oldName, newName))))
      case Lit(lit) =>
        Lit(lit)
      case _ => term
    }

  def alphaConversion(lambda: Lambda, newName: String): Lambda =
    Lambda(newName, rename(lambda.name, newName)(lambda.body))
}
