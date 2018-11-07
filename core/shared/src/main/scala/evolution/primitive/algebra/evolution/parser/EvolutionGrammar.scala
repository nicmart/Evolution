package evolution.primitive.algebra.evolution.parser

import cats.{Defer, MonoidK, Semigroup}
import evolution.geometry.Point
import evolution.primitive.algebra.evolution.{Evolution, parser}
import cats.instances.double._
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.evolution.parser.Debug.debug
import evolution.primitive.algebra.parser.ByVarParser.ByVarParserK
import evolution.primitive.algebra.parser.PrimitiveParsers
import fastparse.noApi.Parser

trait EvolutionExpressions[F[_], R[_]] {
  def chain: ChainExpressions[F, R]
  def constants: ConstantsExpressions[R]
  def binding: BindingExpressions[R]
}

object EvolutionExpressions {
  class Lazy[F[_], R[_]](inner: => EvolutionExpressions[F, R], deferR: Defer[R]) extends EvolutionExpressions[F, R] {
    override def chain: ChainExpressions[F, R] =
      new ChainExpressions.Lazy(inner.chain, deferR)
    override def constants: ConstantsExpressions[R] =
      new ConstantsExpressions.Lazy(inner.constants, deferR)
    override def binding: BindingExpressions[R] =
      new BindingExpressions.Lazy(inner.binding, deferR)
  }

  class Debug[F[_], R[_]](inner: EvolutionExpressions[F, R]) extends EvolutionExpressions[F, R] {
    override def chain: ChainExpressions[F, R] =
      debug("EvolutionExpressions::chain")(new parser.ChainExpressions.Debug(inner.chain))
    override def constants: ConstantsExpressions[R] =
      debug("EvolutionExpressions::constants")(new parser.ConstantsExpressions.Debug(inner.constants))
    override def binding: BindingExpressions[R] =
      debug("EvolutionExpressions::binding")(new parser.BindingExpressions.Debug(inner.binding))
  }
}

trait ChainExpressions[F[_], R[_]] {
  def evolutionOf[T: Semigroup](constant: R[T]): R[F[T]]
}

object ChainExpressions {
  class Lazy[F[_], R[_]](inner: => ChainExpressions[F, R], defer: Defer[R]) extends ChainExpressions[F, R] {
    override def evolutionOf[T: Semigroup](constant: R[T]): R[F[T]] = defer.defer(inner.evolutionOf(constant))
  }

  class Debug[F[_], R[_]](inner: ChainExpressions[F, R]) extends ChainExpressions[F, R] {
    override def evolutionOf[T: Semigroup](constant: R[T]): R[F[T]] =
      debug("ChainExpressions:evolutionOf")(inner.evolutionOf(constant))
  }
}

class ChainGrammar[F[_], R[_]](
  self: EvolutionExpressions[F, R],
  syntax: Chain[F, R],
  override val orMonoid: MonoidK[R],
) extends ChainExpressions[F, R]
    with OrMonoid[R] {
  import syntax._
  override def evolutionOf[T: Semigroup](constant: R[T]): R[F[T]] =
    or(
      empty,
      cons(self.constants.constantOf(constant), self.chain.evolutionOf(constant)),
      mapCons(self.chain.evolutionOf(constant))(
        self.binding.function(
          self.constants.constantOf(constant),
          self.binding.function(self.chain.evolutionOf(constant), self.chain.evolutionOf(constant))
        )
      ),
      mapEmpty(self.chain.evolutionOf(constant), self.chain.evolutionOf(constant))
    )
}

trait ConstantsExpressions[R[_]] {
  def constantOf[T: Semigroup](constant: R[T]): R[T]
  def points: R[Point]
  def doubles: R[Double]
}

object ConstantsExpressions {
  class Lazy[R[_]](inner: => ConstantsExpressions[R], defer: Defer[R]) extends ConstantsExpressions[R] {
    override def constantOf[T: Semigroup](constant: R[T]): R[T] =
      defer.defer(inner.constantOf(constant))
    override def points: R[Point] = defer.defer(inner.points)
    override def doubles: R[Double] = defer.defer(inner.doubles)
  }

  class Debug[R[_]](inner: ConstantsExpressions[R]) extends ConstantsExpressions[R] {
    override def constantOf[T: Semigroup](constant: R[T]): R[T] =
      debug("ConstantsExpressions::constantOf")(inner.constantOf(constant))
    override def points: R[Point] =
      debug("ConstantsExpressions::points")(inner.points)
    override def doubles: R[Double] =
      debug("ConstantsExpressions::doubles")(inner.doubles)
  }
}

class ConstantsGrammar[R[_]](
  self: ConstantsExpressions[R],
  syntax: Constants[R, Unit],
  override val orMonoid: MonoidK[R]
) extends ConstantsExpressions[R]
    with OrMonoid[R] {
  import syntax._
  override def constantOf[T: Semigroup](constant: R[T]): R[T] =
    or(constant, add(self.constantOf(constant), self.constantOf(constant)))
  override def points: R[Point] =
    point(self.constantOf(doubles), self.constantOf(doubles))
  override def doubles: R[Double] =
    syntax.double(())
}

trait BindingExpressions[R[_]] {
  def valueOf[T](t: R[T]): R[T]
  def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2]
}

object BindingExpressions {
  class Lazy[R[_]](inner: => BindingExpressions[R], defer: Defer[R]) extends BindingExpressions[R] {
    override def valueOf[T](t: R[T]): R[T] = defer.defer(inner.valueOf(t))
    override def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] = defer.defer(inner.function(t1, t2))
  }

  class Debug[R[_]](inner: BindingExpressions[R]) extends BindingExpressions[R] {
    override def valueOf[T](t: R[T]): R[T] = debug("BindingExpressions::valueOf")(inner.valueOf(t))
    override def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] =
      debug("BindingExpressions::valueOf")(inner.function(t1, t2))
  }
}

class BindingGrammar[R[_], Var, VarName](
  self: BindingExpressions[R],
  syntax: Binding[R, Var, Unit],
  all: List[R[_]],
  override val orMonoid: MonoidK[R]
) extends BindingExpressions[R]
    with OrMonoid[R] {
  import syntax._

  // TODO allLet and allApp break Generators
  override def valueOf[T](t: R[T]): R[T] =
    or(var0, shift(self.valueOf(t)), fix(self.function(t, t)), allLetExpressions(self.valueOf(t)), allAppExpressions(t))

  override def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] =
    or(lambda(variableSyntax, t2), valueOf(self.function(t1, t2)))

  private def letExpression[T1, T2](t1: R[T1], t2: R[T2]): R[T2] =
    let(variableSyntax, t1, t2)

  private def allLetExpressions[T](t: R[T]): R[T] =
    or(all.map(s => letExpression(s, t)): _*)

  private def appExpression[T1, T2](t1: R[T1], t2: R[T2]): R[T2] =
    app(self.function(t1, t2), t1)

  private def allAppExpressions[T](t: R[T]): R[T] =
    or(all.map(s => appExpression(s, t)): _*)

  private val variableSyntax = syntax.v(())
}

trait OrMonoid[R[_]] {
  def orMonoid: MonoidK[R]
  // TODO Changed to foldRight to improve generators performance.
  def or[T](expressions: R[T]*): R[T] =
    orSeq(expressions)

  protected def orSeq[T](expressions: Seq[R[T]]): R[T] =
    expressions.foldRight(orMonoid.empty[T])(orMonoid.combineK[T])
}

class EvolutionGrammar[F[_], R[_], Var](
  self: EvolutionExpressions[F, R],
  syntax: Evolution[F, R, Unit, Var, Unit],
  variableSyntax: Var,
  override val orMonoid: MonoidK[R]
) extends EvolutionExpressions[F, R]
    with OrMonoid[R] {

  private def doubleLiteral: R[Double] = syntax.constants.double(())

  override def chain: ChainExpressions[F, R] = new ChainExpressions[F, R] {
    override def evolutionOf[T: Semigroup](constant: R[T]): R[F[T]] = {
      println("Executing")
      or(internalChain.evolutionOf(constant), internalBinding.valueOf(self.chain.evolutionOf(constant)))
    }
  }

  override def constants: ConstantsExpressions[R] =
    new ConstantsExpressions[R] {
      override def constantOf[T: Semigroup](constant: R[T]): R[T] =
        or(internalConstants.constantOf(constant), internalBinding.valueOf(self.constants.constantOf(constant)))
      override def points: R[Point] =
        internalConstants.points
      override def doubles: R[Double] =
        internalConstants.doubles
    }

  override def binding: BindingExpressions[R] = new BindingExpressions[R] {
    override def valueOf[T](t: R[T]): R[T] =
      or(t, internalBinding.valueOf(self.binding.valueOf(t)))
    override def function[T1, T2](t1: R[T1], t2: R[T2]): R[T1 => T2] =
      internalBinding.function(t1, t2)
  }

  private lazy val doubles = internalConstants.doubles
  private lazy val points = internalConstants.points

  private lazy val all: List[R[_]] = List[R[_]](
    self.constants.constantOf(doubles),
    self.constants.constantOf(points),
    self.chain.evolutionOf(doubles),
    self.chain.evolutionOf(points)
  )

  private lazy val internalChain: ChainExpressions[F, R] =
    new ChainGrammar(self, syntax.list, orMonoid)

  private lazy val internalConstants: ConstantsExpressions[R] =
    new ConstantsGrammar[R](self.constants, syntax.constants, orMonoid)

  private lazy val internalBinding: BindingExpressions[R] =
    new BindingGrammar(self.binding, syntax.bind, all, orMonoid)
}

object EvolutionGrammar {
  import EvolutionExpressions.Lazy

  def grammar[F[_], R[_]](alg: Evolution[F, R, Double, String, String]): EvolutionExpressions[F, ByVarParserK[R, ?]] = {
    new EvolutionExpressions.Debug[F, ByVarParserK[R, ?]](
      parserGrammarRec[F, R](alg, new Lazy[F, ByVarParserK[R, ?]](grammar(alg), Defer[ByVarParserK[R, ?]]))
    )
  }

  private def parserGrammarRec[F[_], R[_]](
    alg: Evolution[F, R, Double, String, String],
    self: EvolutionExpressions[F, ByVarParserK[R, ?]]
  ): EvolutionExpressions[F, ByVarParserK[R, ?]] = {
    val syntax = new EvolutionSyntax[F, R](alg)
    new EvolutionGrammar[F, ByVarParserK[R, ?], Parser[String]](
      self,
      syntax,
      PrimitiveParsers.varName,
      MonoidK[ByVarParserK[R, ?]]
    )
  }
}

object Debug {
  def debug[T](message: String)(t: => T): T = {
    println(s"START: $message")
    val t2 = t
    println(s"END: $message")
    t2
  }
}
