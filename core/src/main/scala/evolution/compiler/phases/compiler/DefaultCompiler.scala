package evolution.compiler.phases.compiler

import cats.data.NonEmptyList
import cats.implicits._
import evolution.compiler.expression.Expr
import evolution.compiler.phases.compiler.Compilation._
import evolution.compiler.phases.typer.config.{Constant0, Constant1, Constant2, Constant3}
import evolution.compiler.phases.{Compiler, ExprModule}
import evolution.compiler.tree.TreeF._
import evolution.compiler.tree._
import evolution.compiler.types.{Type, Typed}

import scala.collection.immutable.Nil

object DefaultCompiler extends Compiler {
  // TODO module here?
  def compile(tree: TypedTree, module: ExprModule): Either[String, Expr[Any]] =
    compileM(tree).map(module.load).run(CompilerState.empty)

  private def compileM(typedTree: TypedTree): Compilation[Expr[Any]] =
    typedTree.tree match {
      case Id(name, false) =>
        Expr.Var(name).pure[Compilation]

      case Lambda(varName, body) =>
        compileM(body).map(Expr.Lambda(varName, _))

      case Let(varName, value, in) =>
        (compileM(value), compileM(in)).mapN { (compiledValue, compiledIn) =>
          Expr.Let(varName, compiledValue, compiledIn)
        }

      case IntLiteral(n) =>
        typedTree.annotation.value match {
          case Type.Double => Expr.Dbl(n.toDouble).pure[Compilation]
          case _           => Expr.Integer(n).pure[Compilation]
        }

      case DoubleLiteral(n) => // Default to Double for numeric literals
        Expr.Dbl(n.toDouble).pure[Compilation]

      case Bool(b) =>
        Expr.Bool(b).pure[Compilation]

      case Id(Constant0(c), true) =>
        fromEither(c.compile(typedTree.annotation))

      // Arity 0 identifiers
      case Id(id, _) =>
        error(s"Constant $id is not supported as first class value")

      case Lst(ts) =>
        ts.traverse(compileM).map(Expr.Lst(_))

      // Arity 1 identifiers
      case App(AnnotatedTree(_, Id(Constant1(c), true)), NonEmptyList(arg1, Nil)) =>
        for {
          argExpr <- compileTyped(arg1)
          expr <- fromEither(c.compile(argExpr, typedTree.annotation.value))
        } yield expr

      case App(AnnotatedTree(_, Id(Constant2(c), true)), NonEmptyList(arg1, List(arg2))) =>
        for {
          arg1Expr <- compileTyped(arg1)
          arg2Expr <- compileTyped(arg2)
          expr <- fromEither(c.compile(arg1Expr, arg2Expr, typedTree.annotation.value))
        } yield expr

      case App(AnnotatedTree(_, Id(Constant3(c), true)), NonEmptyList(arg1, List(arg2, arg3))) =>
        for {
          arg1Expr <- compileTyped(arg1)
          arg2Expr <- compileTyped(arg2)
          arg3Expr <- compileTyped(arg3)
          expr <- fromEither(c.compile(arg1Expr, arg2Expr, arg3Expr, typedTree.annotation.value))
        } yield expr

      case App(f, args) => (compileM(f), args.traverse(compileM)).mapN(buildAppExpr)

      case _ =>
        error(s"Invalid AST for expression $typedTree")
    }

  private def compileTyped(typedTree: TypedTree): Compilation[Typed[_]] =
    compileM(typedTree).map(Typed(typedTree.annotation.value, _))

  private def buildAppExpr(f: Expr[Any], args: NonEmptyList[Expr[Any]]): Expr[Any] =
    args match {
      case NonEmptyList(arg1, Nil) => Expr.App[Any, Any](f.asExpr, arg1)
      case NonEmptyList(arg1, head :: tail) =>
        buildAppExpr(Expr.App[Any, Any](f.asExpr, arg1), NonEmptyList(head, tail))
    }

  private implicit class CastingOps(value: Any) {
    def asExpr[T]: Expr[T] = value.asInstanceOf[Expr[T]]
  }
}
