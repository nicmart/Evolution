package evolution.compiler.term

import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.term.Compilation._
import evolution.compiler.term.Term.Literal._
import evolution.compiler.term.Term._
import evolution.compiler.tree.{AnnotatedTree, TreeF, TypedTree}
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}

final class TreeToTermCompiler:
  def compile(tree: TypedTree): Either[String, Term] =
    compileM(tree).run(CompilerState.empty)

  private[term] def compileM(typedTree: TypedTree): Compilation[Term] =
    val AnnotatedTree(Qualified(predicates, _), tree) = typedTree

    tree match
      case TreeF.Id(name, _)              => appPredicates(Id(name.string), predicates)
      case TreeF.IntLiteral(n, _)         => appPredicates(Lit(LitInt(n)), predicates)
      case TreeF.DoubleLiteral(n, _)      => appPredicates(Lit(LitDouble(n)), predicates)
      case TreeF.Bool(b, _)               => appPredicates(Lit(LitBool(b)), predicates)
      case TreeF.Lst(ts, _)               => traverse(ts)(compileM).map(ts => Lit(LitList(ts)))
      case TreeF.Lambda(varName, expr, _) => compileM(expr).map(Lambda(varName, _))

      case TreeF.App(f, args, _) =>
        for
          f <- compileM(f)
          args <- traverse(args.toList)(compileM)
        yield app(f, args)

      case TreeF.Let(varName, expr, in, _) =>
        withLocalPredicates(expr.annotation.predicates) {
          for
            exprTerm <- compileM(expr)
            exprTermWithPred <- lambdaFromPredicates(expr.annotation.predicates, exprTerm)
            in <- compileM(in)
          yield Let(varName, exprTermWithPred, in)
        }

  private def lambdaFromPredicates(predicates: List[Predicate], term: Term): Compilation[Term] =
    pVars(predicates).map(lambda(_, term))

  private def pVars(predicates: List[Predicate]): Compilation[List[String]] =
    //traverse(predicates.filter(_.hasTypeVars))(predName)
    traverse(predicates.reverse)(predName)

  private def appPredicates(term: Term, predicates: List[Predicate]): Compilation[Term] =
    traverse(predicates)(argFromPred).map(pArgs => app(term, pArgs))

  private def argFromPred(predicate: Predicate): Compilation[Term] =
    if predicate.hasTypeVars then predName(predicate).map(Id.apply)
    else fromEither(TypingConfig.instance(predicate).map(Inst.apply))

  private def lambda(vars: List[String], term: Term): Term =
    vars.foldLeft(term) { (term, name) =>
      Lambda(name, term)
    }

  private def app(term: Term, args: List[Term]): Term =
    args.foldLeft(term)(Apply.apply)
