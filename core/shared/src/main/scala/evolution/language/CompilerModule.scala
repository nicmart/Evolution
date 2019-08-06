package evolution.language

import cats.data.Kleisli
import cats.implicits._
import cats.mtl.FunctorRaise
import cats.mtl.implicits._
import cats.{ Functor, Monad }
import evolution.data.Expr

trait CompilerModule[F[_]] {

  import AST._
  import TypeClasses._

  type Result[M[_], T] = Kleisli[M, VarContext, T]

  object Compiler {

    def compile[M[_]](expr: AST)(implicit E: FunctorRaise[M, String], M: Monad[M]): Result[M, Expr[expr.Out]] = {
      type K[T] = Result[M, T]

      implicit val KE: FunctorRaise[K, String] = new FunctorRaise[K, String] {
        override val functor: Functor[K] = Functor[K]
        override def raise[A](e: String): K[A] = Kleisli(_ => E.raise[A](e))
      }

      def withVar[A](name: String)(ka: K[A]): K[A] = Kleisli.local[M, A, VarContext](_.push(name))(ka)
      def varContext: K[VarContext] = Kleisli((ctx: VarContext) => ctx.pure[M])

      expr match {
        case Identifier(name, _, false) =>
          varContext.flatMap[Expr[expr.tpe.t.Out]] { ctx =>
            if (ctx.has(name)) (Expr.Var[expr.Out](name): Expr[expr.Out]).pure[K]
            else KE.raise(s"Variable $name is not defined for identifier $expr")
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
          KE.raise(s"Invalid AST for expression $expr")
      }
    }.asInstanceOf[Result[M, Expr[expr.Out]]]

    implicit class CastingOps(value: Any) {
      def asExpr[T]: Expr[T] = value.asInstanceOf[Expr[T]]
      def asExprF[T]: Expr[F[T]] = value.asInstanceOf[Expr[F[T]]]
    }
  }

  class VarContext(vars: List[String]) {
    def has(variable: String): Boolean = vars.contains(variable)
    def indexOf(variable: String): Int = vars.indexOf(variable)
    def push(variable: String): VarContext = new VarContext(variable :: vars)
  }

  object VarContext {
    val empty: VarContext = new VarContext(List.empty)
  }
}
