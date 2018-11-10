package evolution.primitive.algebra.binding.parser

import evolution.primitive.algebra.binding.{Binding, BindingSyntax}
import evolution.primitive.algebra.parser.ByVarParser.{ByVarParserK, Raw}
import evolution.primitive.algebra.parser.ByVarParsers.{function1, function2, function3Dep, infixFlatMap}
import evolution.primitive.algebra.parser.PrimitiveParsers.varUsage
import evolution.primitive.algebra.parser.ParserConfig.White._
import evolution.primitive.algebra.parser.{ByVarParser, PrimitiveParsers}
import fastparse.noApi._
import fastparse.parsers.Combinators.Logged

class BindingParserSyntax[R[_]](alg: Binding[R, String, String])
    extends BindingSyntax[ByVarParserK[R, ?], Parser[String], Unit] {

  override def v(name: Unit): Parser[String] =
    PrimitiveParsers.varName

  override def allVars[T]: ByVarParser[R[T]] =
    ByVarParser.Vars.flatMap(anyVarParser)

  override def var0[A]: ByVarParser[R[A]] =
    Raw(
      vars =>
        vars.headOption.fold[Parser[R[A]]](Fail) { currentVar =>
          varUsage(currentVar).map(_ => alg.var0)
      },
      "var0"
    ).logged("var0")

  override def shift[A](expr: ByVarParser[R[A]]): ByVarParser[R[A]] =
    expr.popVar.map(alg.shift).logged("shift")

  override def let[A, B](
    variableName: Parser[String],
    value: ByVarParser[R[A]],
    expr: ByVarParserK[R, B]
  ): ByVarParserK[R, B] =
    function3Dep[String, R[A], R[B]]("let", Raw(_ => variableName, "let var"), _ => value, {
      case (parsedVariableName, _) => expr.pushVar(parsedVariableName)
    }).map { case (parsedVar, ra, rb) => alg.let(parsedVar, ra, rb) }.logged("let expression")

  override def lambda[A, B](variableName: Parser[String], expr: ByVarParserK[R, B]): ByVarParserK[R, A => B] =
    infixFlatMap[String, R[A => B]](
      Raw(_ => Logged(variableName, s"VariableUsage $variableName", println), "lambda var").logged("lambda var"),
      "->",
      parsedVariableName =>
        expr
          .pushVar(parsedVariableName)
          .map { parsedExpression =>
            alg.lambda[A, B](parsedVariableName, parsedExpression)
          }
          .logged("lambda body")
    )

  override def app[A, B](f: ByVarParserK[R, A => B], a: ByVarParser[R[A]]): ByVarParserK[R, B] =
    function2("app", f, a)
      .map {
        case (parsedFunction, parsedArgument) => alg.app(parsedFunction, parsedArgument)
      }
      .logged("app expr")

  override def fix[A](expr: ByVarParserK[R, A => A]): ByVarParser[R[A]] =
    function1("fix", expr).map(alg.fix).logged("fix expr")

  private def anyVarParser[T](vars: List[String]): ByVarParser[R[T]] =
    vars match {
      case _ :: tail => ByVarParser.Or(List(var0, shift(anyVarParser(tail))))
      case Nil => ByVarParser.Fail()
    }
}
