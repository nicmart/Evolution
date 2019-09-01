package evolution.compilertree.phases.compiling

import cats.data.Kleisli
import cats.implicits._
import evolution.data.Expr
import evolution.compilertree.types.TypeClasses._
import evolution.compilertree.phases.compiling.model.VarContext
import evolution.compilertree.phases.typing.config.{ Constant0, Constant1, Constant2, Constant3 }
import evolution.compilertree.types.Typed
import evolution.compilertree.types.Type
import evolution.materialization.Evolution
import evolution.compilertree.ast.TreeF._
import evolution.compilertree.ast.TreeF

object Compile {

  def compileTree(tree: TypedTree, varContext: VarContext): Either[String, Expr[tree.value.value.Out]] =
    cataCoTree(compileSafe)(tree).run(varContext).asInstanceOf[Either[String, Expr[tree.value.value.Out]]]

  // VarContext => Either[String, T]
  private type Result[T] = Kleisli[Either[String, ?], VarContext, T]

  private def compileSafe(tpe: Qualified[Type], tree: TreeF[Result[Expr[Any]]]): Result[Expr[Any]] =
    tree match {
      case Identifier(name, false) =>
        varContext.flatMap { ctx =>
          if (ctx.has(name)) Expr.Var(name).pure[Result].widen
          else s"Variable $name is not defined for identifier $tree".raiseError[Result, Expr[Any]]
        }

      case Lambda(varName, body) =>
        withVar(varName)(body).map(Expr.Lambda(varName, _))

      case Let(varName, value, in) =>
        (value, withVar(varName)(in)).mapN { (compiledValue, compiledIn) =>
          Expr.Let(varName, compiledValue, compiledIn)
        }

      case IntLiteral(n) =>
        tpe.value match {
          case Type.Dbl => Expr.Dbl(n.toDouble).pure[Result].widen
          case _        => Expr.Integer(n).pure[Result].widen
        }

      case DoubleLiteral(n) => // Default to Double for numeric literals
        Expr.Dbl(n.toDouble).pure[Result].widen

      case Bool(b) =>
        Expr.Bool(b).pure[Result].widen

      case Identifier(Constant0(c), true) =>
        c.compile(tpe).liftTo[Result]

      // Arity 0 identifiers
      case Identifier(id, _) =>
        s"Constant $id is not supported as first class value".raiseError[Result, Expr[Any]]

      case Lst(ts) =>
        ts.sequence.map(Expr.Lst(_))

      // Arity 1 identifiers
      // TODO: here we need contextual information, we need either to change the signature of compileSafe
      // or rethink how we handle predefined constants
      case App(Identifier(Constant1(c), true), x) =>
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

      case App(f, x) =>
        (f, x).mapN { (compiledF, compiledX) =>
          Expr.App(compiledF.asExpr[Any => Any], compiledX.asExpr[Any])
        }

      // TODO this prevents exhaustivity checking
      case _ =>
        s"Invalid AST for expression $tree".raiseError[Result, Expr[Any]]
    }

  private def withVar[A](name: String)(ka: Result[A]): Result[A] =
    Kleisli.local[Either[String, ?], A, VarContext](_.push(name))(ka)

  private def varContext: Result[VarContext] = Kleisli((ctx: VarContext) => Right(ctx))

  implicit class CastingOps(value: Any) {
    def asExpr[T]: Expr[T] = value.asInstanceOf[Expr[T]]
    def asExprF[T]: Expr[Evolution[T]] = value.asInstanceOf[Expr[Evolution[T]]]
  }
}
