package evolution.compiler.phases.compiling

import cats.data.Kleisli
import cats.implicits._
import evolution.data.Expr
import evolution.compiler.tree._
import evolution.compiler.phases.compiling.model.VarContext
import evolution.compiler.phases.typing.config.{ Constant0, Constant1, Constant2, Constant3 }
import evolution.compiler.types.Typed
import evolution.compiler.types.Type
import evolution.materialization.Evolution
import evolution.compiler.tree.TreeF._
import cats.data.NonEmptyList
import scala.collection.immutable.Nil

object Compile {

  def compile(tree: TypedTree, varContext: VarContext): Either[String, Expr[tree.annotation.value.Out]] =
    compileSafe(tree).run(varContext).map(_.asExpr)

  private type Result[T] = Kleisli[Either[String, ?], VarContext, T]

  private def compileSafe(typedTree: TypedTree): Result[Expr[Any]] =
    typedTree.tree match {
      case Identifier(name, false) =>
        varContext.flatMap { ctx =>
          if (ctx.has(name)) Expr.Var(name).pure[Result].widen
          else s"Variable $name is not defined for identifier $typedTree".raiseError[Result, Expr[Any]]
        }

      case Lambda(varName, body) =>
        withVar(varName)(compileSafe(body)).map(Expr.Lambda(varName, _))

      case Let(varName, value, in) =>
        (compileSafe(value), withVar(varName)(compileSafe(in))).mapN { (compiledValue, compiledIn) =>
          Expr.Let(varName, compiledValue, compiledIn)
        }

      case IntLiteral(n) =>
        typedTree.annotation.value match {
          case Type.Dbl => Expr.Dbl(n.toDouble).pure[Result].widen
          case _        => Expr.Integer(n).pure[Result].widen
        }

      case DoubleLiteral(n) => // Default to Double for numeric literals
        Expr.Dbl(n.toDouble).pure[Result].widen

      case Bool(b) =>
        Expr.Bool(b).pure[Result].widen

      case Identifier(Constant0(c), true) =>
        c.compile(typedTree.annotation).liftTo[Result]

      // Arity 0 identifiers
      case Identifier(id, _) =>
        s"Constant $id is not supported as first class value".raiseError[Result, Expr[Any]]

      case Lst(ts) =>
        ts.traverse(compileSafe).map(Expr.Lst(_))

      // Arity 1 identifiers
      case App(AnnotatedTree(_, Identifier(Constant1(c), true)), NonEmptyList(arg1, Nil)) =>
        for {
          argExpr <- compileTyped(arg1)
          expr <- c.compile(argExpr, typedTree.annotation.value).liftTo[Result]
        } yield expr

      case App(AnnotatedTree(_, Identifier(Constant2(c), true)), NonEmptyList(arg1, List(arg2))) =>
        for {
          arg1Expr <- compileTyped(arg1)
          arg2Expr <- compileTyped(arg2)
          expr <- c.compile(arg1Expr, arg2Expr, typedTree.annotation.value).liftTo[Result]
        } yield expr

      case App(AnnotatedTree(_, Identifier(Constant3(c), true)), NonEmptyList(arg1, List(arg2, arg3))) =>
        for {
          arg1Expr <- compileTyped(arg1)
          arg2Expr <- compileTyped(arg2)
          arg3Expr <- compileTyped(arg3)
          expr <- c.compile(arg1Expr, arg2Expr, arg3Expr, typedTree.annotation.value).liftTo[Result]
        } yield expr

      case App(f, args) => (compileSafe(f), args.traverse(compileSafe)).mapN(buildAppExpr)

      case _ =>
        s"Invalid AST for expression $typedTree".raiseError[Result, Expr[Any]]
    }

  private def compileTyped(typedTree: TypedTree): Result[Typed[Expr[Any]]] =
    compileSafe(typedTree).map(Typed(typedTree.annotation.value, _))

  private def buildAppExpr(f: Expr[Any], args: NonEmptyList[Expr[Any]]): Expr[Any] =
    args match {
      case NonEmptyList(arg1, Nil) => Expr.App[Any, Any](f.asExpr, arg1)
      case NonEmptyList(arg1, head :: tail) =>
        buildAppExpr(Expr.App[Any, Any](f.asExpr, arg1), NonEmptyList(head, tail))
    }

  private def withVar[A](name: String)(ka: Result[A]): Result[A] =
    Kleisli.local[Either[String, ?], A, VarContext](ctx => ctx.push(name))(ka)

  private def varContext: Result[VarContext] = Kleisli((ctx: VarContext) => Right(ctx))

  private implicit class CastingOps(value: Any) {
    def asExpr[T]: Expr[T] = value.asInstanceOf[Expr[T]]
    def asExprF[T]: Expr[Evolution[T]] = value.asInstanceOf[Expr[Evolution[T]]]
  }
}
