package evolution.compiler.term

import Term.*
import evolution.compiler.term.Term.Literal.LitList
import evolution.compiler.term.CaptureAvoidingRenamer.Renamed

/** Transform a term in order to avoid capture-related problems in substitutions. See for example
  * https://en.wikipedia.org/wiki/Lambda_calculus#Capture-avoiding_substitutions
  */
class CaptureAvoidingRenamer:
  import CaptureAvoidingRenamer.*, CaptureAvoidingRenamer.Renamed.*
  def rename(term: Term, vars: Set[String] = Set.empty): Term = renameM(term).run(vars)

  private def renameM(term: Term): Renamed[Term] = term match
    case Lit(LitList(ts)) => traverse(ts)(renameM).map(ts => Lit(LitList(ts)))

    case Let(name, expr, body) =>
      for
        newName <- freshVar(name)
        renamedExpr <- renameM(expr)
        b <- withVar(newName)(renameM(body))
        bodyWithName = TermRenamer.rename(name, newName)(b)
      yield Let(newName, renamedExpr, bodyWithName)

    case Lambda(name, body) =>
      for
        newName <- freshVar(name)
        b <- withVar(newName)(renameM(body))
        bodyWithNewName = TermRenamer.rename(name, newName)(b)
      yield Lambda(newName, bodyWithNewName)

    case Apply(f, x) =>
      for
        f <- renameM(f)
        x <- renameM(x)
      yield Apply(f, x)

    case _ => pure(term)

object CaptureAvoidingRenamer:
  type VarCounter = Int

  private case class Renamed[T](run: Set[String] => T):
    def flatMap[U](f: T => Renamed[U]): Renamed[U] =
      Renamed { vars =>
        f(run(vars)).run(vars)
      }

    def map[U](f: T => U): Renamed[U] = flatMap(f andThen Renamed.pure)

  private object Renamed:
    def pure[T](t: T): Renamed[T] = Renamed(_ => t)

    def freshVar(name: String): Renamed[String] =
      Renamed { vars =>
        if vars.contains(name) then freshVar(name + "'").run(vars)
        else name
      }

    def withVar[T](name: String)(ft: => Renamed[T]): Renamed[T] =
      Renamed { vars =>
        ft.run(vars + name)
      }

    def traverse[T, U](ts: List[T])(f: T => Renamed[U]): Renamed[List[U]] =
      ts.foldRight[Renamed[List[U]]](pure(Nil)) { (t, renamed) =>
        renamed.flatMap(tail => f(t).map(u => u :: tail))
      }
