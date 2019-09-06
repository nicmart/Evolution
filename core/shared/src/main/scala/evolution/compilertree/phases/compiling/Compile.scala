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
import cats.data.NonEmptyList
import scala.collection.immutable.Nil

object Compile {

  def compileTree(tree: TypedTree, varContext: VarContext): Either[String, Expr[tree.value.value.Out]] =
    ???

  def compileTree2(tree: TypedTree, varContext: VarContext): Either[String, Expr[tree.value.value.Out]] =
    ???
  //cataCoTree(compileSafe)(tree).run(varContext).asInstanceOf[Either[String, Expr[tree.value.value.Out]]]

  // VarContext => Either[String, T]
  case class Env(varContext: VarContext, expectedType: Type)
  private type Result[T] = Kleisli[Either[String, ?], Env, T]
  case class TypedResult(tpe: Qualified[Type], result: Result[Expr[Any]]) {
    def typedResult: Result[Typed[Expr[Any]]] =
      result.map(Typed(tpe.value, _))
  }

  def compileSafe(tree: TreeF[CoTree[TypedResult]]): Result[Expr[Any]] =
    tree match {
      case Identifier(name, false) =>
        varContext.flatMap { ctx =>
          if (ctx.has(name)) Expr.Var(name).pure[Result].widen
          else s"Variable $name is not defined for identifier $tree".raiseError[Result, Expr[Any]]
        }

      case Lambda(varName, body) =>
        withVar(varName)(body.value.result).map(Expr.Lambda(varName, _))

      case Let(varName, value, in) =>
        (value.value.result, withVar(varName)(in.value.result)).mapN { (compiledValue, compiledIn) =>
          Expr.Let(varName, compiledValue, compiledIn)
        }

      case IntLiteral(n) =>
        expectedType.map[Expr[Any]] {
          case Type.Dbl => Expr.Dbl(n.toDouble)
          case _        => Expr.Integer(n)
        }

      case DoubleLiteral(n) => // Default to Double for numeric literals
        Expr.Dbl(n.toDouble).pure[Result].widen

      case Bool(b) =>
        Expr.Bool(b).pure[Result].widen

      case Identifier(Constant0(c), true) =>
        expectedType.flatMap(tpe => c.compile(Qualified(tpe)).liftTo[Result])

      // Arity 0 identifiers
      case Identifier(id, _) =>
        s"Constant $id is not supported as first class value".raiseError[Result, Expr[Any]]

      case Lst(ts) =>
        ts.map(_.value.result).sequence.map(Expr.Lst(_))

      // Arity 1 identifiers
      // TODO: here we need contextual information, we need either to change the signature of compileSafe
      // or rethink how we handle predefined constants
      case App(CoTree(_, Identifier(Constant1(c), true)), NonEmptyList(CoTree(arg1Result, _), Nil)) =>
        for {
          expectedType <- expectedType
          argExpr <- arg1Result.typedResult
          expr <- c.compile(argExpr, expectedType).liftTo[Result]
        } yield expr

      // case App(App(Identifier(Constant2(c), _, true), x, _), y, typeOut) =>
      //   for {
      //     compiledX <- compileSafe(x)
      //     compiledY <- compileSafe(y)
      //     result <- c
      //       .compile(Typed(x.qualifiedType.value, compiledX), Typed(y.qualifiedType.value, compiledY), typeOut.value)
      //       .liftTo[Result]
      //   } yield result

      case App(
          CoTree(_, Identifier(Constant2(c), true)),
          NonEmptyList(CoTree(arg1Result, _), List(CoTree(arg2Result, _)))
          ) =>
        for {
          expectedType <- expectedType
          arg1Expr <- arg1Result.typedResult
          arg2Expr <- arg2Result.typedResult
          expr <- c.compile(arg1Expr, arg2Expr, expectedType).liftTo[Result]
        } yield expr

      // // Arity 3 identifiers
      // case App(App(App(Identifier(Constant3(c), _, true), x, _), y, _), z, typeOut) =>
      //   for {
      //     compiledX <- compileSafe(x)
      //     compiledY <- compileSafe(y)
      //     compiledZ <- compileSafe(z)
      //     result <- c
      //       .compile(
      //         Typed(x.qualifiedType.value, compiledX),
      //         Typed(y.qualifiedType.value, compiledY),
      //         Typed(z.qualifiedType.value, compiledZ),
      //         typeOut.value
      //       )
      //       .liftTo[Result]
      //   } yield result

      case App(
          CoTree(_, Identifier(Constant3(c), true)),
          NonEmptyList(CoTree(arg1Result, _), List(CoTree(arg2Result, _), CoTree(arg3Result, _)))
          ) =>
        for {
          expectedType <- expectedType
          arg1Expr <- arg1Result.typedResult
          arg2Expr <- arg2Result.typedResult
          arg3Expr <- arg3Result.typedResult
          expr <- c.compile(arg1Expr, arg2Expr, arg3Expr, expectedType).liftTo[Result]
        } yield expr

      case App(f, args) => (f.value.result, args.map(_.value.result).sequence).mapN(buildAppExpr)

      // case App(f, x) =>
      //   (f, x).mapN { (compiledF, compiledX) =>
      //     Expr.App(compiledF.asExpr[Any => Any], compiledX.asExpr[Any])
      //   }

      // TODO this prevents exhaustivity checking
      case _ =>
        s"Invalid AST for expression $tree".raiseError[Result, Expr[Any]]
    }

  private def buildAppExpr(f: Expr[Any], args: NonEmptyList[Expr[Any]]): Expr[Any] =
    args match {
      case NonEmptyList(arg1, Nil) => Expr.App[Any, Any](f.asExpr, arg1)
      case NonEmptyList(arg1, head :: tail) =>
        buildAppExpr(Expr.App[Any, Any](f.asExpr, arg1), NonEmptyList(head, tail))
    }

  private def withVar[A](name: String)(ka: Result[A]): Result[A] =
    Kleisli.local[Either[String, ?], A, Env](env => env.copy(varContext = env.varContext.push(name)))(ka)

  private def withExpectedType[A](tpe: Type)(ka: Result[A]): Result[A] =
    Kleisli.local[Either[String, ?], A, Env](env => env.copy(expectedType = tpe))(ka)

  private def varContext: Result[VarContext] = Kleisli((env: Env) => Right(env.varContext))
  private def expectedType: Result[Type] = Kleisli((env: Env) => Right(env.expectedType))

  implicit class CastingOps(value: Any) {
    def asExpr[T]: Expr[T] = value.asInstanceOf[Expr[T]]
    def asExprF[T]: Expr[Evolution[T]] = value.asInstanceOf[Expr[Evolution[T]]]
  }
}
