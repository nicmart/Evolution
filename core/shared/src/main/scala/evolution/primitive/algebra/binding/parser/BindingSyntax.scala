package evolution.primitive.algebra.binding.parser

import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.parser.ByVarParser.{ByVarParserK, Raw}
import evolution.primitive.algebra.parser.ByVarParsers.{function1, function2, function3Dep, infixFlatMap}
import evolution.primitive.algebra.parser.PrimitiveParsers.varUsage
import evolution.primitive.algebra.parser.ParserConfig.White._
import evolution.primitive.algebra.parser.{ByVarParser, ByVarParsers, PrimitiveParsers}
import fastparse.noApi._

class BindingSyntax[R[_]](alg: Binding[R, String, String]) extends Binding[ByVarParserK[R, ?], Parser[String], Unit] {

  override def v(name: Unit): Parser[String] =
    PrimitiveParsers.varName

  override def var0[A]: ByVarParser[R[A]] = Raw { vars =>
    vars.headOption.fold[Parser[R[A]]](Fail) { currentVar =>
      varUsage(currentVar).map(_ => alg.var0)
    }
  }

  override def shift[A](expr: ByVarParser[R[A]]): ByVarParser[R[A]] = Raw {
    case _ :: tail => expr.parser(tail).map(alg.shift)
    case _ => Fail
  }

  override def let[A, B](
    variableName: Parser[String],
    value: ByVarParser[R[A]],
    expr: ByVarParserK[R, B]
  ): ByVarParserK[R, B] =
    function3Dep[String, R[A], R[B]]("let", Raw(_ => variableName), _ => value, {
      case (parsedVariableName, _) => expr.withVar(parsedVariableName)
    }).map { case (parsedVar, ra, rb) => alg.let(parsedVar, ra, rb) }

  override def lambda[A, B](variableName: Parser[String], expr: ByVarParserK[R, B]): ByVarParserK[R, A => B] =
    infixFlatMap[String, R[A => B]](
      Raw(_ => variableName),
      "->",
      parsedVariableName =>
        expr.withVar(parsedVariableName).map { parsedExpression =>
          alg.lambda(parsedVariableName, parsedExpression)
      }
    )

  override def app[A, B](f: ByVarParserK[R, A => B], a: ByVarParser[R[A]]): ByVarParserK[R, B] =
    function2("app", f, a).map {
      case (parsedFunction, parsedArgument) => alg.app(parsedFunction, parsedArgument)
    }

  override def fix[A](expr: ByVarParserK[R, A => A]): ByVarParser[R[A]] =
    function1("fix", expr).map(alg.fix)
}
