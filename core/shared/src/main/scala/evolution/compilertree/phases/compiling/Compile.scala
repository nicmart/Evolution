package evolution.compilertree.phases.compiling

import cats.data.Kleisli
import cats.implicits._
import evolution.data.Expr
import evolution.compiler.types.TypeClasses._
import evolution.compiler.ast.AST
import evolution.compiler.ast.AST._
import evolution.compiler.phases.compiling.model.VarContext
import evolution.compiler.phases.typing.config.{ Constant0, Constant1, Constant2, Constant3 }
import evolution.compiler.types.Typed
import evolution.compiler.types.Type
import evolution.materialization.Evolution

object Compile {

  def compile(ast: AST, varContext: VarContext): Either[String, Expr[ast.Out]] =
    compileSafe(ast).run(varContext).asInstanceOf[Either[String, Expr[ast.Out]]]

  // VarContext => Either[String, T]
  private type Result[T] = Kleisli[Either[String, ?], VarContext, T]

  private def compileSafe(ast: AST): Result[Expr[Any]] =
    ast match {
      case Identifier(name, _, false) =>
        varContext.flatMap { ctx =>
          if (ctx.has(name)) Expr.Var(name).pure[Result].widen
          else s"Variable $name is not defined for identifier $ast".raiseError[Result, Expr[Any]]
        }

      case Lambda(varName, body, _) =>
        withVar(varName.name)(compileSafe(body)).map(Expr.Lambda(varName.name, _))

      case Let(varName, value, in, _) =>
        (compileSafe(value), withVar(varName.name)(compileSafe(in))).mapN { (compiledValue, compiledIn) =>
          Expr.Let(varName.name, compiledValue, compiledIn)
        }

      case IntLiteral(n, Qualified(_, Type.Integer)) =>
        Expr.Integer(n).pure[Result].widen

      case IntLiteral(n, Qualified(_, Type.Dbl)) =>
        Expr.Dbl(n.toDouble).pure[Result].widen

      // TODO only compile if it is typed as a double
      case DoubleLiteral(n, _) => // Default to Double for numeric literals
        Expr.Dbl(n.toDouble).pure[Result].widen

      case Bool(b, _) =>
        Expr.Bool(b).pure[Result].widen

      case Identifier(Constant0(c), qualifiedType, true) =>
        c.compile(qualifiedType).liftTo[Result]

      // Arity 0 identifiers
      case Identifier(id, _, _) =>
        s"Constant $id is not supported as first class value".raiseError[Result, Expr[Any]]

      case Lst(ts, _) =>
        ts.traverse(compileSafe).map(Expr.Lst(_))

      // Arity 1 identifiers
      case App(Identifier(Constant1(c), _, true), x, typeOut) =>
        compileSafe(x).flatMap(
          compiledX => c.compile(Typed(x.qualifiedType.value, compiledX), typeOut.value).liftTo[Result]
        )

      case App(App(Identifier(Constant2(c), _, true), x, _), y, typeOut) =>
        for {
          compiledX <- compileSafe(x)
          compiledY <- compileSafe(y)
          result <- c
            .compile(Typed(x.qualifiedType.value, compiledX), Typed(y.qualifiedType.value, compiledY), typeOut.value)
            .liftTo[Result]
        } yield result

      // Arity 3 identifiers
      case App(App(App(Identifier(Constant3(c), _, true), x, _), y, _), z, typeOut) =>
        for {
          compiledX <- compileSafe(x)
          compiledY <- compileSafe(y)
          compiledZ <- compileSafe(z)
          result <- c
            .compile(
              Typed(x.qualifiedType.value, compiledX),
              Typed(y.qualifiedType.value, compiledY),
              Typed(z.qualifiedType.value, compiledZ),
              typeOut.value
            )
            .liftTo[Result]
        } yield result

      case App(f, x, _) =>
        (compileSafe(f), compileSafe(x)).mapN { (compiledF, compiledX) =>
          Expr.App(compiledF.asExpr[Any => Any], compiledX.asExpr[Any])
        }

      // TODO this prevents exhaustivity checking
      case _ =>
        s"Invalid AST for expression $ast".raiseError[Result, Expr[Any]]
    }

  private def withVar[A](name: String)(ka: Result[A]): Result[A] =
    Kleisli.local[Either[String, ?], A, VarContext](_.push(name))(ka)

  private def varContext: Result[VarContext] = Kleisli((ctx: VarContext) => Right(ctx))

  implicit class CastingOps(value: Any) {
    def asExpr[T]: Expr[T] = value.asInstanceOf[Expr[T]]
    def asExprF[T]: Expr[Evolution[T]] = value.asInstanceOf[Expr[Evolution[T]]]
  }
}
