package evolution.primitive.algebra.binding.parser

import evolution.primitive.algebra.binding.{ Binding, BindingSyntax }
import evolution.primitive.algebra.parser.{ ByVarParser, PrimitiveParsers }
import evolution.primitive.algebra.parser.ByVarParser.{ ByVarParserK, Raw }
import evolution.primitive.algebra.parser.ByVarParsers.{ function1, function2, function3Dep, infixFlatMap }
import evolution.primitive.algebra.parser.ParserConfig.White._
import evolution.primitive.algebra.parser.PrimitiveParsers.varUsage
import fastparse.noApi
import fastparse.noApi._

class BindingParserSyntax[R[_]](alg: Binding[R, String]) extends BindingSyntax[ByVarParserK[R, ?], Parser[String]] {

  override def allVars: noApi.Parser[String] =
    PrimitiveParsers.varName

  override def allVarsExpressions[T]: ByVarParser[R[T]] =
    ByVarParser.Vars.flatMap(anyVarParser)

  override def var0[A](name: String): ByVarParser[R[A]] =
    Raw(
      vars =>
        vars.headOption.fold[Parser[R[A]]](Fail) { currentVar =>
          varUsage(currentVar).map(_ => alg.var0(name))
      },
      "var0"
    )

  override def shift[A](expr: ByVarParser[R[A]]): ByVarParser[R[A]] =
    expr.popVar.map(alg.shift)

  override def let[A, B](
    variableName: Parser[String],
    value: ByVarParser[R[A]],
    expr: ByVarParserK[R, B]
  ): ByVarParserK[R, B] =
    function3Dep[String, R[A], R[B]]("let", Raw(_ => variableName, "let var"), _ => value, {
      case (parsedVariableName, _) => expr.pushVar(parsedVariableName)
    }).map { case (parsedVar, ra, rb) => alg.let(parsedVar, ra, rb) }

  override def lambda[A, B](variableName: Parser[String], expr: ByVarParserK[R, B]): ByVarParserK[R, A => B] =
    infixFlatMap[String, R[A => B]](
      Raw(_ => variableName, s"VariableUsage: $variableName"),
      "->",
      parsedVariableName =>
        expr.pushVar(parsedVariableName).map { parsedExpression =>
          alg.lambda[A, B](parsedVariableName, parsedExpression)
      }
    )

  override def app[A, B](f: ByVarParserK[R, A => B], a: ByVarParser[R[A]]): ByVarParserK[R, B] =
    function2("app", f, a).map {
      case (parsedFunction, parsedArgument) => alg.app(parsedFunction, parsedArgument)
    }

  override def fix[A](expr: ByVarParserK[R, A => A]): ByVarParser[R[A]] =
    function1("fix", expr).map(alg.fix)

  private def anyVarParser[T](vars: List[String]): ByVarParser[R[T]] =
    vars match {
      case _ :: tail => ByVarParser.Or(List(var0(""), shift(anyVarParser(tail))))
      case Nil       => ByVarParser.Fail()
    }
}
