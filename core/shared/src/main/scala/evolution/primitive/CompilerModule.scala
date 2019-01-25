package evolution.primitive
import evolution.primitive.algebra.evolution.Evolution
import cats.implicits._

trait CompilerModule[F[_]] { self: WithAst[F] =>
  import ast._

  object Compiler {
    def compile[R[_]](expr: Expr, alg: Evolution[F, R]): Either[String, R[expr.Out]] = {
      def compile(expr: Expr): Either[String, R[expr.Out]] = {
        expr match {
          case Expr.Var(name, tpe) =>
            Right(alg.bind.var0[expr.Out](name))
          case fc @ Expr.FuncCall(funcId, args, tpe) =>
            compileFuncCall(fc)
          case Expr.Lambda(varName, body, tpe) =>
            compile(body).map(alg.bind.lambda(varName.name, _))
          case Expr.Let(varName, value, in, tpe) =>
            for {
              compiledValue <- compile(value)
              compiledIn <- compile(in)
            } yield alg.bind.let(varName.name, compiledValue, compiledIn)
          case Expr.Number(n, Type.Dbl) =>
            Right(alg.constants.double(n.toDouble))
          case _ =>
            Left("Invalid type")
        }
      }.asInstanceOf[Either[String, R[expr.Out]]]

      def compileFuncCall(f: Expr.FuncCall): Either[String, R[f.Out]] = ???

      compile(expr)
    }
  }

}
