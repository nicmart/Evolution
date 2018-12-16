package evolution.primitive.algebra.chain.interpreter
import evolution.algebra.representation.RNGRepr
import evolution.data.Annotation
import evolution.data.Annotation.Info.Unknown
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.evolution.interpreter.EvolutionExpr

// TODO through applicative?
object ChainAnnotator extends Chain[RNGRepr, Annotation] {
  private val builder = new EvolutionExpr[RNGRepr]

  override def empty[A]: Annotation[RNGRepr[A]] =
    Annotation(Set.empty, Unknown(builder.chain.empty))

  override def cons[A](evalHead: Annotation[A], evalTail: Annotation[RNGRepr[A]]): Annotation[RNGRepr[A]] =
    Annotation(evalHead.vars ++ evalTail.vars, Unknown(builder.chain.cons(evalHead.expr, evalTail.expr)))

  override def mapEmpty[A](eva: Annotation[RNGRepr[A]], eva2: Annotation[RNGRepr[A]]): Annotation[RNGRepr[A]] =
    Annotation(eva.vars ++ eva2.vars, Unknown(builder.chain.mapEmpty(eva.expr, eva2.expr)))

  override def mapCons[A, B](evalFA: Annotation[RNGRepr[A]])(
    evalF: Annotation[A => RNGRepr[A] => RNGRepr[B]]): Annotation[RNGRepr[B]] =
    Annotation(evalFA.vars ++ evalF.vars, Unknown(builder.chain.mapCons(evalFA.expr)(evalF.expr)))
}
