package evolution.primitive
import evolution.primitive.Ast

object typer {
  class Typer[F[_]](ast: Ast[F]) {
    import ast._
    def check(ctx: Context, expected: Type, expr: Expr) = ???
  }
}
