package evolution.compiler.term

import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.term.Compilation._
import evolution.compiler.term.Term.Literal._
import evolution.compiler.term.Term.PArg.{PInst, PVar}
import evolution.compiler.term.Term._
import evolution.compiler.tree.{AnnotatedTree, TreeF, TypedTree}
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClassInstance.NumericInst
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}

class TreeToTermCompiler {
  def compile(tree: TypedTree): Either[String, Term] =
    compileM(tree).run(CompilerState.empty)

  private[term] def compileM(typedTree: TypedTree): Compilation[Term] = {
    val AnnotatedTree(Qualified(predicates, tpe), tree) = typedTree

    tree match {
      case TreeF.Id(name, primitive) =>
        traverse(predicates)(argFromPred).map(pArgs => pApp(Id(name), pArgs))
      case TreeF.Lambda(varName, expr)  => ???
      case TreeF.App(f, args)           => ???
      case TreeF.Let(varName, expr, in) => ???

      case TreeF.IntLiteral(n) =>
        predicates match {
          case List(predicate @ Predicate("Num", List(pTpe))) if tpe == pTpe =>
            argFromPred(predicate).map(pArg => PApp(Lit(LitInt(n)), pArg))
          case _ => error(s"Unexpected Type Error for Int Literal")
        }

      case TreeF.DoubleLiteral(n) => pure(Lit(LitDouble(n)))
      case TreeF.Bool(b)          => pure(Lit(LitBool(b)))

      case TreeF.Lst(ts) => traverse(ts)(compileM).map(ts => Lit(LitList(ts)))
    }
  }

  private def argFromPred(predicate: Predicate): Compilation[PArg] =
    if (predicate.hasTypeVars) predName(predicate).map(PVar.apply)
    else fromEither(TypingConfig.instance(predicate).map(PInst.apply))

  private def pApp(term: Term, pArgs: List[PArg]): Term =
    pArgs.foldLeft(term)(PApp)
}

object TreeToTermCompiler {
  type PredName = String
}
