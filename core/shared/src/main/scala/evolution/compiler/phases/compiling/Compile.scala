package evolution.compiler.phases.compiling

import cats.data.Kleisli
import cats.implicits._
import cats.mtl.FunctorRaise
import cats.mtl.implicits._
import cats.{ Functor, Monad }
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

  type Result[M[_], T] = Kleisli[M, VarContext, T]

  def compile[M[_]](ast: AST)(implicit E: FunctorRaise[M, String], M: Monad[M]): Result[M, Expr[ast.Out]] = {
    type K[T] = Result[M, T]

    implicit val KE: FunctorRaise[K, String] = new FunctorRaise[K, String] {
      override val functor: Functor[K] = Functor[K]
      override def raise[A](e: String): K[A] = Kleisli(_ => E.raise[A](e))
    }

    def withVar[A](name: String)(ka: K[A]): K[A] = Kleisli.local[M, A, VarContext](_.push(name))(ka)
    def varContext: K[VarContext] = Kleisli((ctx: VarContext) => ctx.pure[M])

    ast match {
      case Identifier(name, _, false) =>
        varContext.flatMap[Expr[ast.tpe.t.Out]] { ctx =>
          if (ctx.has(name)) (Expr.Var[ast.Out](name): Expr[ast.Out]).pure[K]
          else KE.raise(s"Variable $name is not defined for identifier $ast")
        }

      case Lambda(varName, body, _) =>
        withVar(varName)(compile[M](body)).map(Expr.Lambda(varName, _))

      case Let(varName, value, in, _) =>
        (compile[M](value), withVar(varName)(compile[M](in))).mapN { (compiledValue, compiledIn) =>
          Expr.Let(varName, compiledValue, compiledIn)
        }

      case IntLiteral(n, Qualified(_, Type.Integer)) =>
        Expr.Integer(n).pure[K]

      case IntLiteral(n, Qualified(_, Type.Dbl)) =>
        Expr.Dbl(n.toDouble).pure[K]

      case DoubleLiteral(n, _) => // Default to Double for numeric literals
        Expr.Dbl(n.toDouble).pure[K]

      case Bool(b, _) =>
        Expr.Bool(b).pure[K]

      case Identifier(Constant0(c), tpe, true) =>
        c.compile[K](tpe)

      // Arity 0 identifiers
      case Identifier(id, _, _) =>
        s"Constant $id is not supported as first class value".raise[K, Expr[Any]]

      // Arity 1 identifiers
      case App(Identifier(Constant1(c), _, true), x, typeOut) =>
        compile[M](x).flatMap(compiledX => c.compile[K](Typed(x.tpe.t, compiledX), typeOut.t))

      // Arity 2 identifiers
      case App(App(Identifier(Constant2(c), _, true), x, _), y, typeOut) =>
        for {
          compiledX <- compile[M](x)
          compiledY <- compile[M](y)
          result <- c.compile[K](Typed(x.tpe.t, compiledX), Typed(y.tpe.t, compiledY), typeOut.t)
        } yield result

      // Arity 3 identifiers
      case App(App(App(Identifier(Constant3(c), _, true), x, _), y, _), z, typeOut) =>
        for {
          compiledX <- compile[M](x)
          compiledY <- compile[M](y)
          compiledZ <- compile[M](z)
          result <- c
            .compile[K](Typed(x.tpe.t, compiledX), Typed(y.tpe.t, compiledY), Typed(z.tpe.t, compiledZ), typeOut.t)
        } yield result

      case App(f, x, _) =>
        (compile[M](f), compile[M](x)).mapN { (compiledF, compiledX) =>
          Expr.App(compiledF.asExpr[Any => Any], compiledX.asExpr[Any])
        }

      case _ =>
        KE.raise(s"Invalid AST for expression $ast")
    }
  }.asInstanceOf[Result[M, Expr[ast.Out]]]

  implicit class CastingOps(value: Any) {
    def asExpr[T]: Expr[T] = value.asInstanceOf[Expr[T]]
    def asExprF[T]: Expr[Evolution[T]] = value.asInstanceOf[Expr[Evolution[T]]]
  }
}
