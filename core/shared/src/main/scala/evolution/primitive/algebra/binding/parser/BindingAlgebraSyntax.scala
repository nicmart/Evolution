package evolution.primitive.algebra.binding.parser

import evolution.primitive.algebra.ByVarParser
import evolution.primitive.algebra.binding.BindingAlgebra
import evolution.primitive.algebra.parser.PrimitiveParsers.{function1, function2, functionFlatMap, varUsage}
import evolution.primitive.algebra.parser.ParserConfig.White._
import evolution.primitive.algebra.parser.PrimitiveParsers
import fastparse.noApi._

class BindingAlgebraSyntax[R[_]](alg: BindingAlgebra[R, String])
    extends BindingAlgebra[ByVarParser[R, ?], Parser[String]] {

  val variableIdentifier: Parser[String] = PrimitiveParsers.varName

  override def varName(name: String): Parser[String] =
    P(name).!

  override def var0[A]: ByVarParser[R, A] =
    vars =>
      vars.headOption.fold[Parser[R[A]]](Fail) { currentVar =>
        varUsage(currentVar).map(_ => alg.var0)
    }

  override def shift[A](expr: ByVarParser[R, A]): ByVarParser[R, A] = {
    case _ :: tail => expr(tail).map(alg.shift)
    case _ => Fail
  }

  override def let[A, B](variableName: Parser[String], value: ByVarParser[R, A])(
    expr: ByVarParser[R, B]
  ): ByVarParser[R, B] =
    vars =>
      functionFlatMap[(String, R[A]), R[B]](function2("let", variableName, value(vars)), {
        case (parsedVariableName, parsedValue) =>
          expr(parsedVariableName :: vars).map { parsedExpression =>
            alg.let(parsedVariableName, parsedValue)(parsedExpression)
          }
      })

  override def lambda[A, B](variableName: Parser[String], expr: ByVarParser[R, B]): ByVarParser[R, A => B] =
    vars =>
      functionFlatMap[String, R[A => B]](
        function1("lambda", variableName),
        parsedVariableName =>
          expr(parsedVariableName :: vars).map { parsedExpression =>
            alg.lambda(parsedVariableName, parsedExpression)
        }
    )

  override def app[A, B](f: ByVarParser[R, A => B], a: ByVarParser[R, A]): ByVarParser[R, B] =
    vars =>
      function2("app", f(vars), a(vars)).map {
        case (parsedFunction, parsedArgument) => alg.app(parsedFunction, parsedArgument)
    }

  override def fix[A](expr: ByVarParser[R, A => A]): ByVarParser[R, A] =
    vars => function1("fix", expr(vars)).map(alg.fix)
}
