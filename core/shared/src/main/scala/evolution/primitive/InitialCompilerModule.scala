package evolution.primitive

import cats.{ MonadError, Traverse }
import cats.implicits._
import evolution.data.Initial
import evolution.geometry
import evolution.primitive.algebra.derived.Derived

trait InitialCompilerModule[F[_]] { self: WithAst[F] =>
  val initial = new Initial[F] {}
  import initial._

  val derived: Derived[F, R] = initial.evolution.derived

  object Compiler {
    import ast._
    import PredefinedFunction._
    def compile[M[_]](expr: Expr, ctx: VarContext)(implicit M: MonadError[M, String]): M[R[expr.Out]] = {
      expr match {
        case Expr.Var(name, tpe) =>
          Var0(name).pure[M]
        case fc @ Expr.FuncCall(funcId, args, tpe) =>
          compileFuncCall[M](fc, ctx)
        case Expr.Lambda(varName, body, tpe) =>
          compile[M](body, ctx.push(varName.name)).map(Lambda(varName.name, _))
        case Expr.Let(varName, value, in, tpe) =>
          for {
            compiledValue <- compile[M](value, ctx)
            compiledIn <- compile[M](in, ctx.push(varName.name))
          } yield Let(varName.name, compiledValue, compiledIn)
        case Expr.Number(n, Type.Dbl) =>
          Dbl(n.toDouble).pure[M]
        case Expr.Number(n, Type.Integer) =>
          Integer(n.toInt).pure[M]
        case _ =>
          M.raiseError(s"Invalid type for expression $expr")
      }
    }.asInstanceOf[M[R[expr.Out]]]

    def compileFuncCall[M[_]](func: Expr.FuncCall, ctx: VarContext)(implicit M: MonadError[M, String]): M[R[func.Out]] = {
      (func.funcId, func.args) match {
        case (Point, x :: y :: Nil) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            initial.Pnt(compiledX.asInstanceOf[R[Double]], compiledY.asInstanceOf[R[Double]])
          }
        case (X, p :: Nil) =>
          compile[M](p, ctx).map { compiledP =>
            initial.X(compiledP.asInstanceOf[R[geometry.Point]])
          }
        case (Y, p :: Nil) =>
          compile[M](p, ctx).map { compiledP =>
            initial.Y(compiledP.asInstanceOf[R[geometry.Point]])
          }
        case (Floor, d :: Nil) =>
          compile[M](d, ctx).map(compiledD => initial.Floor(compiledD.asInstanceOf[R[Double]]))
        case (Add, x :: y :: Nil) =>
          (M.fromEither(Type.group(func.tpe)), compile[M](x, ctx), compile[M](y, ctx)).mapN {
            (sg, compiledX, compiledY) =>
              initial.Add(compiledX.asInstanceOf[R[func.Out]], compiledY.asInstanceOf[R[func.Out]])(sg)
          }
        case (Div, x :: y :: Nil) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            initial.Div(compiledX.asInstanceOf[R[Double]], compiledY.asInstanceOf[R[Double]])
          }
        case (Exp, x :: y :: Nil) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            initial.Exp(compiledX.asInstanceOf[R[Double]], compiledY.asInstanceOf[R[Double]])
          }
        case (Inverse, x :: Nil) =>
          (M.fromEither(Type.group(func.tpe)), compile[M](x, ctx)).mapN { (g, compiledX) =>
            initial.Inverse(compiledX.asInstanceOf[R[func.Out]])(g)
          }
        case (Multiply, x :: y :: Nil) =>
          (M.fromEither(Type.vectorSpace(func.tpe)), compile[M](x, ctx), compile[M](y, ctx)).mapN {
            (vs, compiledX, compiledY) =>
              initial.Multiply(compiledX.asInstanceOf[R[Double]], compiledY.asInstanceOf[R[func.Out]])(vs)
          }
        case (Cos, x :: Nil) =>
          compile[M](x, ctx).map(compiledX => initial.Cos(compiledX.asInstanceOf[R[Double]]))
        case (Sin, x :: Nil) =>
          compile[M](x, ctx).map(compiledX => initial.Sin(compiledX.asInstanceOf[R[Double]]))
        case (Eq, x :: y :: Nil) =>
          for {
            compiledX <- compile[M](x, ctx)
            compiledY <- compile[M](y, ctx)
            eqTypeClass <- M.fromEither(Type.eqTypeClass(x.tpe))
          } yield initial.Equals[x.Out](compiledX, compiledY.asInstanceOf[R[x.Out]])(eqTypeClass)
        case (If, x :: y :: z :: Nil) =>
          (compile[M](x, ctx), compile[M](y, ctx), compile[M](z, ctx)).mapN { (compiledX, compiledY, compiledZ) =>
            initial.IfThen(compiledX.asInstanceOf[R[Boolean]], compiledY, compiledZ.asInstanceOf[R[y.Out]])
          }
        case (Fix, f :: Nil) =>
          compile[M](f, ctx).map { compiledF =>
            initial.Fix(compiledF.asInstanceOf[R[func.Out => func.Out]])
          }
        case (App, f :: x :: Nil) =>
          (compile[M](f, ctx), compile[M](x, ctx)).mapN { (compiledF, compiledX) =>
            initial.App(compiledF.asInstanceOf[R[x.Out => func.Out]], compiledX.asInstanceOf[R[x.Out]])
          }
        case (Empty, Nil) =>
          initial.Empty.pure[M]
        case (Cons, x :: y :: Nil) => // TODO I am not sure if we can assume transitivity and remove redundant constraints
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            initial.Cons[x.Out](compiledX.asInstanceOf[R[x.Out]], compiledY.asInstanceOf[R[F[x.Out]]])
          }
        case (MapEmpty, x :: y :: Nil) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            initial.MapEmpty[func.Out](compiledX.asInstanceOf[R[F[func.Out]]], compiledY.asInstanceOf[R[F[func.Out]]])
          }
        case (MapCons, x :: f :: Nil) =>
          (compile[M](x, ctx), compile[M](f, ctx)).mapN { (compiledX, compiledF) =>
            initial.MapCons[x.Out, func.Out](
              compiledX.asInstanceOf[R[F[x.Out]]],
              compiledF.asInstanceOf[R[x.Out => F[x.Out] => F[func.Out]]])
          }
        case (Cartesian, x :: y :: Nil) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            derived.cartesian(compiledX.asInstanceOf[R[F[Double]]], compiledY.asInstanceOf[R[F[Double]]])
          }
        case (Polar, x :: y :: Nil) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            derived.polar(compiledX.asInstanceOf[R[F[Double]]], compiledY.asInstanceOf[R[F[Double]]])
          }
        case (Constant, x :: Nil) =>
          compile[M](x, ctx).map(compiledX => derived.constant(compiledX.asInstanceOf[R[func.Out]]))
        case (Integrate, x :: y :: Nil) =>
          for {
            compiledX <- compile[M](x, ctx)
            compiledY <- compile[M](y, ctx)
            vs <- M.fromEither(Type.vectorSpace(x.tpe))
          } yield derived.integrate[x.Out](compiledX, compiledY.asInstanceOf[R[F[x.Out]]])(vs)
        case (Solve1, x :: y :: Nil) =>
          for {
            compiledX <- compile[M](x, ctx)
            compiledY <- compile[M](y, ctx)
            vs <- M.fromEither(Type.vectorSpace(y.tpe))
          } yield
            derived.solve1[y.Out](compiledX.asInstanceOf[R[F[y.Out => y.Out]]], compiledY.asInstanceOf[R[y.Out]])(vs)
        case (Solve2, x :: y :: z :: Nil) =>
          for {
            compiledX <- compile[M](x, ctx)
            compiledY <- compile[M](y, ctx)
            compiledZ <- compile[M](z, ctx)
            vs <- M.fromEither(Type.vectorSpace(y.tpe))
          } yield
            derived.solve2[y.Out](
              compiledX.asInstanceOf[R[F[y.Out => y.Out => y.Out]]],
              compiledY.asInstanceOf[R[y.Out]],
              compiledY.asInstanceOf[R[y.Out]]
            )(vs)
        case (Concat, x :: y :: Nil) => // TODO gen new vars
          (M.fromEither(Type.unwrapF(func.tpe)), compile[M](x, ctx), compile[M](y, ctx)).mapN {
            (innerType, compiledX, compiledY) =>
              derived.concat[innerType.Out](
                compiledX.asInstanceOf[R[F[innerType.Out]]],
                compiledY.asInstanceOf[R[F[innerType.Out]]]
              )
          }
        case (Map, x :: f :: Nil) =>
          (compile[M](x, ctx), compile[M](f, ctx)).mapN { (compiledX, compiledF) =>
            derived.map[x.Out, func.Out](
              compiledX.asInstanceOf[R[F[x.Out]]],
              compiledF.asInstanceOf[R[x.Out => func.Out]])
          }
        case (FlatMap, x :: f :: Nil) =>
          (M.fromEither(Type.unwrapF(func.tpe)), compile[M](x, ctx), compile[M](f, ctx)).mapN {
            (innerType, compiledX, compiledF) =>
              derived.flatMap[x.Out, innerType.Out](
                compiledX.asInstanceOf[R[F[x.Out]]],
                compiledF.asInstanceOf[R[x.Out => F[innerType.Out]]]
              )
          }
        case (Take, n :: e :: Nil) =>
          (M.fromEither(Type.unwrapF(func.tpe)), compile[M](n, ctx), compile[M](e, ctx)).mapN {
            (innerType, compiledN, compiledF) =>
              derived.take(compiledN.asInstanceOf[R[Int]], compiledF.asInstanceOf[R[F[innerType.Out]]])
          }
        case (Uniform, from :: to :: Nil) =>
          (compile[M](from, ctx), compile[M](to, ctx)).mapN { (compiledFrom, compiledTo) =>
            initial.Uniform(compiledFrom.asInstanceOf[R[Double]], compiledTo.asInstanceOf[R[Double]])
          }
        case (UniformDiscrete, from :: to :: step :: Nil) =>
          (compile[M](from, ctx), compile[M](to, ctx), compile[M](step, ctx)).mapN {
            (compiledFrom, compiledTo, compiledStep) =>
              initial.UniformDiscrete(
                compiledFrom.asInstanceOf[R[Double]],
                compiledTo.asInstanceOf[R[Double]],
                compiledStep.asInstanceOf[R[Double]])
          }
        case (UniformChoice, choices) =>
          choices.traverse(choice => compile[M](choice, ctx).asInstanceOf[M[Any]]).map { compiledChoices =>
            initial.UniformChoice(compiledChoices.asInstanceOf[List[R[func.Out]]])
          }
        case _ => M.raiseError(s"Invalid type for expression $func")
      }
    }.asInstanceOf[M[R[func.Out]]]
  }

  class VarContext(vars: List[String]) {
    def indexOf(variable: String): Int = vars.indexOf(variable)
    def push(variable: String): VarContext = new VarContext(variable :: vars)
  }

  object VarContext {
    val empty: VarContext = new VarContext(List.empty)
  }
}
