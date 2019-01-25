package evolution.primitive
import evolution.primitive.algebra.evolution.Evolution

trait CompilerModule[F[_]] { self: WithAst[F] =>
  import ast._

  object Compiler {
    def compile[R[_]](expr: Expr, alg: Evolution[F, R]): Either[String, R[_]] =
      expr match {
        case Expr.Var(name, tpe)               => ???
        case Expr.FuncCall(funcId, args, tpe)  => ???
        case Expr.Lambda(varName, body, tpe)   => ???
        case Expr.Let(varName, value, in, tpe) => ???
        case Expr.Number(n, Type.Dbl)          => Right(alg.constants.double(n.toDouble))
        case Expr.Number(n, _)                 => Left("Invalid type for a number")
      }
  }
}
