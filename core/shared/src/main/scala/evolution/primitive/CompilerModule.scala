package evolution.primitive

import cats.{ MonadError, Traverse }
import cats.implicits._
import evolution.data.ExpressionModule
import evolution.geometry
import evolution.data.WithExpression

// TODO Random extensions and self types, please to do something better
trait CompilerModule[F[_]] extends DesugarModule[F] with WithExpression[F] { self: WithAst[F] =>

  import expressionModule._
  import Desugarer._

  object Compiler {
    import ast._
    import PredefinedFunction._
    def compile[M[_]](expr: AST, ctx: VarContext)(implicit M: MonadError[M, String]): M[Expr[expr.Out]] = {
      expr match {
        case AST.Var(name, tpe) =>
          VarN(ctx.indexOf(name), name).pure[M]
        case fc @ AST.FuncCall(funcId, args, tpe) =>
          compileFuncCall[M](fc, ctx)
        case AST.Lambda(varName, body, tpe) =>
          compile[M](body, ctx.push(varName.name)).map(Lambda(varName.name, _))
        case AST.Let(varName, value, in, tpe) =>
          for {
            compiledValue <- compile[M](value, ctx)
            compiledIn <- compile[M](in, ctx.push(varName.name))
          } yield Let(varName.name, compiledValue, compiledIn)
        case AST.Number(n, Type.Dbl) =>
          Dbl(n.toDouble).pure[M]
        case AST.Number(n, Type.Integer) =>
          Integer(n.toInt).pure[M]
        case _ =>
          M.raiseError(s"Invalid type for expression $expr")
      }
    }.asInstanceOf[M[Expr[expr.Out]]]

    def compileFuncCall[M[_]](func: AST.FuncCall, ctx: VarContext)(
      implicit M: MonadError[M, String]): M[Expr[func.Out]] = {
      (func.funcId, func.args) match {
        case (Point, x :: y :: Nil) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            expressionModule.Pnt(compiledX.asInstanceOf[Expr[Double]], compiledY.asInstanceOf[Expr[Double]])
          }
        case (X, p :: Nil) =>
          compile[M](p, ctx).map { compiledP =>
            expressionModule.X(compiledP.asInstanceOf[Expr[geometry.Point]])
          }
        case (Y, p :: Nil) =>
          compile[M](p, ctx).map { compiledP =>
            expressionModule.Y(compiledP.asInstanceOf[Expr[geometry.Point]])
          }
        case (Floor, d :: Nil) =>
          compile[M](d, ctx).map(compiledD => expressionModule.Floor(compiledD.asInstanceOf[Expr[Double]]))

        case (Add, x :: y :: Nil) =>
          func.tpe match {
            // Overload + for evolutions
            case Type.Evo(tpe) =>
              (M.fromEither(Type.group(tpe)), compile[M](x, ctx), compile[M](y, ctx)).mapN {
                (sg, compiledX, compiledY) =>
                  addEvo(compiledX.asInstanceOf[Expr[F[tpe.Out]]], compiledY.asInstanceOf[Expr[F[tpe.Out]]])(sg)
              }
            case tpe =>
              (M.fromEither(Type.group(func.tpe)), compile[M](x, ctx), compile[M](y, ctx)).mapN {
                (sg, compiledX, compiledY) =>
                  expressionModule.Add(compiledX.asInstanceOf[Expr[func.Out]], compiledY.asInstanceOf[Expr[func.Out]])(
                    sg)
              }
          }

        case (Div, x :: y :: Nil) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            expressionModule.Div(compiledX.asInstanceOf[Expr[Double]], compiledY.asInstanceOf[Expr[Double]])
          }
        case (Exp, x :: y :: Nil) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            expressionModule.Exp(compiledX.asInstanceOf[Expr[Double]], compiledY.asInstanceOf[Expr[Double]])
          }
        case (Inverse, x :: Nil) =>
          func.tpe match {
            // Overload - for evolutions
            case Type.Evo(tpe) =>
              (M.fromEither(Type.group(tpe)), compile[M](x, ctx)).mapN { (group, compiledX) =>
                inverseEvo(compiledX.asInstanceOf[Expr[F[tpe.Out]]])(group)
              }
            case tpe =>
              (M.fromEither(Type.group(func.tpe)), compile[M](x, ctx)).mapN { (g, compiledX) =>
                expressionModule.Inverse(compiledX.asInstanceOf[Expr[func.Out]])(g)
              }
          }

        case (Multiply, x :: y :: Nil) =>
          func.tpe match {
            // Overload * for evolutions
            // This cannot be overloaded, due to the constraint put by the typer on k,
            // case Type.Evo(tpe) =>
            //   (M.fromEither(Type.vectorSpace(tpe)), compile[M](x, ctx), compile[M](y, ctx)).mapN {
            //     (vs, compiledX, compiledY) =>
            //       multEvo(compiledX.asInstanceOf[R[F[Double]]], compiledY.asInstanceOf[R[F[tpe.Out]]])(vs)
            //   }
            case tpe =>
              (M.fromEither(Type.vectorSpace(func.tpe)), compile[M](x, ctx), compile[M](y, ctx)).mapN {
                (vs, compiledX, compiledY) =>
                  expressionModule.Multiply(
                    compiledX.asInstanceOf[Expr[Double]],
                    compiledY.asInstanceOf[Expr[func.Out]])(vs)
              }
          }

        case (Cos, x :: Nil) =>
          compile[M](x, ctx).map(compiledX => expressionModule.Cos(compiledX.asInstanceOf[Expr[Double]]))
        case (Sin, x :: Nil) =>
          compile[M](x, ctx).map(compiledX => expressionModule.Sin(compiledX.asInstanceOf[Expr[Double]]))
        case (PI, Nil) =>
          M.pure(expressionModule.Dbl(Math.PI))
        case (Mod, x :: y :: Nil) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (cx, cy) =>
            expressionModule.Mod(cx.asInstanceOf[Expr[Double]], cy.asInstanceOf[Expr[Double]])
          }
        case (Eq, x :: y :: Nil) =>
          for {
            compiledX <- compile[M](x, ctx)
            compiledY <- compile[M](y, ctx)
            eqTypeClass <- M.fromEither(Type.eqTypeClass(x.tpe))
          } yield expressionModule.Equals[x.Out](compiledX, compiledY.asInstanceOf[Expr[x.Out]])(eqTypeClass)
        case (If, x :: y :: z :: Nil) =>
          (compile[M](x, ctx), compile[M](y, ctx), compile[M](z, ctx)).mapN { (compiledX, compiledY, compiledZ) =>
            expressionModule.IfThen(
              compiledX.asInstanceOf[Expr[Boolean]],
              compiledY,
              compiledZ.asInstanceOf[Expr[y.Out]])
          }
        case (Fix, f :: Nil) =>
          compile[M](f, ctx).map { compiledF =>
            expressionModule.Fix(compiledF.asInstanceOf[Expr[func.Out => func.Out]])
          }
        case (App, f :: x :: Nil) =>
          (compile[M](f, ctx), compile[M](x, ctx)).mapN { (compiledF, compiledX) =>
            expressionModule.App(compiledF.asInstanceOf[Expr[x.Out => func.Out]], compiledX.asInstanceOf[Expr[x.Out]])
          }
        case (Empty, Nil) =>
          println("emptyness inside me")
          expressionModule.Empty().pure[M]
        case (Cons, x :: y :: Nil) => // TODO I am not sure if we can assume transitivity and remove redundant constraints
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            expressionModule.Cons[x.Out](compiledX.asInstanceOf[Expr[x.Out]], compiledY.asInstanceOf[Expr[F[x.Out]]])
          }
        case (MapEmpty, x :: y :: Nil) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            expressionModule
              .MapEmpty[func.Out](compiledX.asInstanceOf[Expr[F[func.Out]]], compiledY.asInstanceOf[Expr[F[func.Out]]])
          }
        case (MapCons, x :: f :: Nil) =>
          (compile[M](x, ctx), compile[M](f, ctx)).mapN { (compiledX, compiledF) =>
            expressionModule.MapCons[x.Out, func.Out](
              compiledX.asInstanceOf[Expr[F[x.Out]]],
              compiledF.asInstanceOf[Expr[x.Out => F[x.Out] => F[func.Out]]]
            )
          }
        case (Cartesian, x :: y :: Nil) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            cartesian(compiledX.asInstanceOf[Expr[F[Double]]], compiledY.asInstanceOf[Expr[F[Double]]])
          }
        case (Polar, x :: y :: Nil) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            polar(compiledX.asInstanceOf[Expr[F[Double]]], compiledY.asInstanceOf[Expr[F[Double]]])
          }
        case (Constant, x :: Nil) =>
          compile[M](x, ctx).map(compiledX => constant(compiledX.asInstanceOf[Expr[func.Out]]))
        case (Integrate, x :: y :: Nil) =>
          for {
            compiledX <- compile[M](x, ctx)
            compiledY <- compile[M](y, ctx)
            vs <- M.fromEither(Type.vectorSpace(x.tpe))
          } yield integrate[x.Out](compiledX, compiledY.asInstanceOf[Expr[F[x.Out]]])(vs)
        case (Solve1, x :: y :: Nil) =>
          for {
            compiledX <- compile[M](x, ctx)
            compiledY <- compile[M](y, ctx)
            vs <- M.fromEither(Type.vectorSpace(y.tpe))
          } yield
            solve1[y.Out](compiledX.asInstanceOf[Expr[F[y.Out => y.Out]]], compiledY.asInstanceOf[Expr[y.Out]])(vs)
        case (Solve2, x :: y :: z :: Nil) =>
          for {
            compiledX <- compile[M](x, ctx)
            compiledY <- compile[M](y, ctx)
            compiledZ <- compile[M](z, ctx)
            vs <- M.fromEither(Type.vectorSpace(y.tpe))
          } yield
            solve2[y.Out](
              compiledX.asInstanceOf[Expr[F[y.Out => y.Out => y.Out]]],
              compiledY.asInstanceOf[Expr[y.Out]],
              compiledY.asInstanceOf[Expr[y.Out]]
            )(vs)
        case (Concat, x :: y :: Nil) => // TODO gen new vars
          (M.fromEither(Type.unwrapF(func.tpe)), compile[M](x, ctx), compile[M](y, ctx)).mapN {
            (innerType, compiledX, compiledY) =>
              concat[innerType.Out](
                compiledX.asInstanceOf[Expr[F[innerType.Out]]],
                compiledY.asInstanceOf[Expr[F[innerType.Out]]]
              )
          }
        case (Map, x :: f :: Nil) =>
          (compile[M](x, ctx), compile[M](f, ctx)).mapN { (compiledX, compiledF) =>
            map[x.Out, func.Out](
              compiledX.asInstanceOf[Expr[F[x.Out]]],
              compiledF.asInstanceOf[Expr[x.Out => func.Out]]
            )
          }
        case (FlatMap, x :: f :: Nil) =>
          (M.fromEither(Type.unwrapF(func.tpe)), compile[M](x, ctx), compile[M](f, ctx)).mapN {
            (innerType, compiledX, compiledF) =>
              flatMap[x.Out, innerType.Out](
                compiledX.asInstanceOf[Expr[F[x.Out]]],
                compiledF.asInstanceOf[Expr[x.Out => F[innerType.Out]]]
              )
          }
        case (Take, n :: e :: Nil) =>
          (M.fromEither(Type.unwrapF(func.tpe)), compile[M](n, ctx), compile[M](e, ctx)).mapN {
            (innerType, compiledN, compiledF) =>
              take(compiledN.asInstanceOf[Expr[Int]], compiledF.asInstanceOf[Expr[F[innerType.Out]]])
          }

        case (ZipWith, a :: b :: f :: Nil) =>
          (compile[M](a, ctx), compile[M](b, ctx), compile[M](f, ctx)).mapN { (compiledA, compiledB, compiledF) =>
            zipWith(
              compiledA.asInstanceOf[Expr[F[Any]]],
              compiledB.asInstanceOf[Expr[F[Any]]],
              compiledF.asInstanceOf[Expr[Any => Any => Any]])
          }

        case (Uniform, from :: to :: Nil) =>
          (compile[M](from, ctx), compile[M](to, ctx)).mapN { (compiledFrom, compiledTo) =>
            expressionModule.Uniform(compiledFrom.asInstanceOf[Expr[Double]], compiledTo.asInstanceOf[Expr[Double]])
          }
        case (UniformDiscrete, from :: to :: step :: Nil) =>
          (compile[M](from, ctx), compile[M](to, ctx), compile[M](step, ctx)).mapN {
            (compiledFrom, compiledTo, compiledStep) =>
              expressionModule.UniformDiscrete(
                compiledFrom.asInstanceOf[Expr[Double]],
                compiledTo.asInstanceOf[Expr[Double]],
                compiledStep.asInstanceOf[Expr[Double]]
              )
          }
        case (UniformChoice, choices) =>
          choices.traverse(choice => compile[M](choice, ctx).asInstanceOf[M[Any]]).map { compiledChoices =>
            expressionModule.UniformChoice(compiledChoices.asInstanceOf[List[Expr[func.Out]]])
          }
        case _ => M.raiseError(s"Invalid type for expression $func")
      }
    }.asInstanceOf[M[Expr[func.Out]]]
  }

  class VarContext(vars: List[String]) {
    def indexOf(variable: String): Int = vars.indexOf(variable)
    def push(variable: String): VarContext = new VarContext(variable :: vars)
  }

  object VarContext {
    val empty: VarContext = new VarContext(List.empty)
  }
}
