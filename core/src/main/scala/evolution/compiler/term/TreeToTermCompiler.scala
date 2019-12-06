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
      case TreeF.Id(name, primitive) => appPredicates(Id(name), predicates)
      case TreeF.IntLiteral(n)       => appPredicates(Lit(LitInt(n)), predicates)
      case TreeF.DoubleLiteral(n)    => appPredicates(Lit(LitDouble(n)), predicates)
      case TreeF.Bool(b)             => appPredicates(Lit(LitBool(b)), predicates)

      case TreeF.Lambda(varName, expr) => ???
      case TreeF.App(f, args)          => ???
      case TreeF.Let(varName, expr, in) =>
        withLocalPredicates(expr.annotation.predicates) {
          for {
            exprTerm <- compileM(expr)
            exprTermWithPred <- lambdaFromPredicates(expr.annotation.predicates, exprTerm)
            in <- compileM(in)
          } yield Let(varName, exprTermWithPred, in)
        }

      case TreeF.Lst(ts) => traverse(ts)(compileM).map(ts => Lit(LitList(ts)))
    }
  }

  private def lambdaFromPredicates(predicates: List[Predicate], term: Term): Compilation[Term] =
    pVars(predicates).map(pLambda(_, term))

  private def pVars(predicates: List[Predicate]): Compilation[List[String]] =
    traverse(predicates.filter(_.hasTypeVars))(predName)

  private def appPredicates(term: Term, predicates: List[Predicate]): Compilation[Term] =
    traverse(predicates)(argFromPred).map(pArgs => pApp(term, pArgs))

  private def argFromPred(predicate: Predicate): Compilation[PArg] =
    if (predicate.hasTypeVars) predName(predicate).map(PVar.apply)
    else fromEither(TypingConfig.instance(predicate).map(PInst.apply))

  private def pLambda(vars: List[String], term: Term): Term =
    vars.foldLeft(term) { (term, name) =>
      PLambda(name, term)
    }

  private def pApp(term: Term, pArgs: List[PArg]): Term =
    pArgs.foldLeft(term)(PApp)
}

object TreeToTermCompiler {
  type PredName = String
}
