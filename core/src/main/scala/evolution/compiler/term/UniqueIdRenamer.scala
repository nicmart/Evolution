package evolution.compiler.term

import Term._
import evolution.compiler.term.Term.Literal.LitList
import evolution.compiler.term.UniqueIdRenamer.Renamed

class UniqueIdRenamer {
  import UniqueIdRenamer._, UniqueIdRenamer.Renamed._
  def rename(term: Term): Term = renameM(term).runA(0)

  private def renameM(term: Term): Renamed[Term] = term match {
    case Lit(LitList(ts)) => traverse(ts)(renameM).map(ts => Lit(LitList(ts)))

    case Let(name, expr, body) =>
      for {
        renamedExpr <- renameM(expr)
        newName <- freshVar
        bodyWithName = TermRenamer.rename(name, newName)(body)
        renamedBody <- renameM(bodyWithName)
      } yield Let(newName, renamedExpr, renamedBody)

    case Lambda(name, body) =>
      for {
        newName <- freshVar
        bodyWithNewName = TermRenamer.rename(name, newName)(body)
        renamedBody <- renameM(bodyWithNewName)
      } yield Lambda(newName, renamedBody)

    case App(f, x) =>
      for {
        f <- renameM(f)
        x <- renameM(x)
      } yield App(f, x)

    case _ => pure(term)
  }
}

object UniqueIdRenamer {
  type VarCounter = Int

  private case class Renamed[T](run: VarCounter => (VarCounter, T)) {
    def runA(counter: VarCounter): T = run(counter)._2
    def flatMap[U](f: T => Renamed[U]): Renamed[U] =
      Renamed { counter =>
        val (newCounter, t) = run(counter)
        f(t).run(newCounter)
      }
    def map[U](f: T => U): Renamed[U] = flatMap(f andThen Renamed.pure)
  }

  private object Renamed {
    def pure[T](t: T): Renamed[T] = Renamed(s => (s, t))
    def freshVar: Renamed[String] = Renamed(s => (s + 1, s"$$$s"))
    def traverse[T, U](ts: List[T])(f: T => Renamed[U]): Renamed[List[U]] =
      ts.foldRight[Renamed[List[U]]](pure(Nil)) { (t, renamed) =>
        renamed.flatMap(tail => f(t).map(u => u :: tail))
      }
  }
}
