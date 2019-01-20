package evolution.primitive
import evolution.primitive.Ast

import scala.util.Try

class Typer[F[_]](val ast: Ast[F]) {
  import ast._
  def check(ctx: Context, expected: Type, expr: Expr): Either[String, Expr] =
    (expr, expected) match {
      case (Expr.Var(name, tpe), _) =>
        Left("")
      case (Expr.FuncCall(funcName, args, tpe), _) =>
        Left("")
      case (Expr.BinaryOp(op, a, b, tpe), _) =>
        Left("")
      case (Expr.Lambda(varName, body, tpe), _) =>
        Left("")

      case (Expr.Number(n, tpe), Type.Dbl) =>
        parseDouble(n).map(_ => Expr.Number(n, Typed(Type.Dbl)))
      case (Expr.Number(n, tpe), Type.Integer) =>
        parseInt(n).map(_ => Expr.Number(n, Typed(Type.Integer)))
    }

  private def parseInt(n: String): Either[String, Int] =
    Try(n.toInt).toEither.left.map(_ => "")

  private def parseDouble(n: String): Either[String, Double] =
    Try(n.toDouble).toEither.left.map(_ => "")
}
