package evolution.compiler.phases.compiler

import cats.implicits._
import evolution.compiler.expression.Expr
import evolution.compiler.tree._
import evolution.compiler.phases.typer.config.{ Constant0, Constant1, Constant2, Constant3 }
import evolution.compiler.phases.Compiler
import evolution.compiler.types.Typed
import evolution.compiler.types.TypeT
import evolution.materialization.Evolution
import evolution.compiler.tree.TreeF._
import cats.data.NonEmptyList
import scala.collection.immutable.Nil
import evolution.compiler.module.Module

object DefaultCompiler extends Compiler {
  // TODO module here?
  def compile(tree: TypedTree, module: Module): Either[String, Expr[_]] =
    compileSafe(tree).map(module.load)

  private def compileSafe(typedTree: TypedTree): Either[String, Expr[Any]] =
    typedTree.tree match {
      case Identifier(name, false) =>
        Expr.Var(name).asRight

      case Lambda(varName, body) =>
        compileSafe(body).map(Expr.Lambda(varName, _))

      case Let(varName, value, in) =>
        (compileSafe(value), compileSafe(in)).mapN { (compiledValue, compiledIn) =>
          Expr.Let(varName, compiledValue, compiledIn)
        }

      case IntLiteral(n) =>
        typedTree.annotation.value match {
          case TypeT.Double => Expr.Dbl(n.toDouble).asRight
          case _            => Expr.Integer(n).asRight
        }

      case DoubleLiteral(n) => // Default to Double for numeric literals
        Expr.Dbl(n.toDouble).asRight

      case Bool(b) =>
        Expr.Bool(b).asRight

      case Identifier(Constant0(c), true) =>
        c.compile(typedTree.annotation)

      // Arity 0 identifiers
      case Identifier(id, _) =>
        s"Constant $id is not supported as first class value".asLeft

      case Lst(ts) =>
        ts.traverse(compileSafe).map(Expr.Lst(_))

      // Arity 1 identifiers
      case App(AnnotatedTree(_, Identifier(Constant1(c), true)), NonEmptyList(arg1, Nil)) =>
        for {
          argExpr <- compileTyped(arg1)
          expr <- c.compile(argExpr, typedTree.annotation.value)
        } yield expr

      case App(AnnotatedTree(_, Identifier(Constant2(c), true)), NonEmptyList(arg1, List(arg2))) =>
        for {
          arg1Expr <- compileTyped(arg1)
          arg2Expr <- compileTyped(arg2)
          expr <- c.compile(arg1Expr, arg2Expr, typedTree.annotation.value)
        } yield expr

      case App(AnnotatedTree(_, Identifier(Constant3(c), true)), NonEmptyList(arg1, List(arg2, arg3))) =>
        for {
          arg1Expr <- compileTyped(arg1)
          arg2Expr <- compileTyped(arg2)
          arg3Expr <- compileTyped(arg3)
          expr <- c.compile(arg1Expr, arg2Expr, arg3Expr, typedTree.annotation.value)
        } yield expr

      case App(f, args) => (compileSafe(f), args.traverse(compileSafe)).mapN(buildAppExpr)

      case _ =>
        s"Invalid AST for expression $typedTree".asLeft
    }

  private def compileTyped(typedTree: TypedTree): Either[String, Typed[_]] =
    compileSafe(typedTree).map(Typed(typedTree.annotation.value, _))

  private def buildAppExpr(f: Expr[Any], args: NonEmptyList[Expr[Any]]): Expr[Any] =
    args match {
      case NonEmptyList(arg1, Nil) => Expr.App[Any, Any](f.asExpr, arg1)
      case NonEmptyList(arg1, head :: tail) =>
        buildAppExpr(Expr.App[Any, Any](f.asExpr, arg1), NonEmptyList(head, tail))
    }

  private implicit class CastingOps(value: Any) {
    def asExpr[T]: Expr[T] = value.asInstanceOf[Expr[T]]
    def asExprF[T]: Expr[Evolution[T]] = value.asInstanceOf[Expr[Evolution[T]]]
  }
}
