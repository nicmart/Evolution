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
      case App(f, x) =>
        App(rename(oldName, newName)(f), rename(oldName, newName)(x))
      case PLambda(pName, body) =>
        PLambda(pName, if (pName == oldName) body else rename(oldName, newName)(body))
      case PApp(term, arg) =>
        PApp(rename(oldName, newName)(term), renamePArg(oldName, newName)(arg))
      case Lit(LitList(ts)) =>
        Lit(LitList(ts.map(rename(oldName, newName))))
      case Lit(lit) =>
        Lit(lit)
    }

  private def renamePArg(oldName: String, newName: String)(pArg: PArg): PArg = pArg match {
    case PArg.PVar(name)      => PArg.PVar(if (name == oldName) newName else name)
    case PArg.PInst(instance) => PArg.PInst(instance)
  }

}
