package evolution.primitive

import cats.arrow.Arrow
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

    def compile[M[_]](expr: AST, ctx: VarContext)(implicit M: MonadError[M, String]): M[Expr[expr.Out]] = {
      expr match {
        case AST.Var(name, tpe) =>
          VarN(ctx.indexOf(name), name).pure[M]

        case AST.Const(PredefinedConstant.PI, _) =>
          M.pure(expressionModule.Dbl(Math.PI))

        case AST.Const(PredefinedConstant.Empty, _) =>
          M.pure(expressionModule.Empty())

        case fc @ AST.Const(id, tpe) =>
          M.raiseError(s"Constant $id is not supported as first class value")

        case AST.Lambda(varName, body, tpe) =>
          compile[M](body, ctx.push(varName.name)).map(Lambda(varName.name, _))

        case AST.Let(varName, value, in, tpe) =>
          for {
            compiledValue <- compile[M](value, ctx)
            compiledIn <- compile[M](in, ctx.push(varName.name))
          } yield Let(varName.name, compiledValue, compiledIn)

        case AST.Number(n, Type.Integer) =>
          Integer(n.toInt).pure[M]

        case AST.Number(n, _) => // Default to Double for numeric literals
          Dbl(n.toDouble).pure[M]

        case App(PredefinedConstant.Fix, x) =>
          compile[M](x, ctx).map { compiledX =>
            expressionModule.Fix(compiledX.asExpr[Any => Any])
          }

        case App2(PredefinedConstant.Cons, head, tail) =>
          (compile[M](head, ctx), compile[M](tail, ctx)).mapN { (head, tail) =>
            expressionModule.Cons(head.asExpr, tail.asExprF)
          }

        case App2(PredefinedConstant.MapEmpty, a, b) =>
          (compile[M](a, ctx), compile[M](b, ctx)).mapN { (compiledA, compiledB) =>
            expressionModule.MapEmpty(compiledA.asExprF, compiledB.asExprF)
          }

        case App2(PredefinedConstant.MapCons, a, f) =>
          (compile[M](a, ctx), compile[M](f, ctx)).mapN { (compiledA, compiledF) =>
            expressionModule.MapCons(
              compiledA.asExprF,
              compiledF.asExpr[Any => F[Any] => F[Any]]
            )
          }

        case App2(PredefinedConstant.Point, x, y) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            expressionModule.Pnt(compiledX.asExpr, compiledY.asExpr)
          }
        case App(PredefinedConstant.X, p) =>
          compile[M](p, ctx).map { compiledP =>
            expressionModule.X(compiledP.asExpr)
          }
        case App(PredefinedConstant.Y, p) =>
          compile[M](p, ctx).map { compiledP =>
            expressionModule.Y(compiledP.asExpr)
          }
        case App(PredefinedConstant.Floor, d) =>
          compile[M](d, ctx).map(compiledD => expressionModule.Floor(compiledD.asExpr))

        case App2(PredefinedConstant.Add, x, y) =>
          x.tpe match {
            // Overload + for evolutions
            case Type.Evo(tpe) =>
              (M.fromEither(Type.group(tpe)), compile[M](x, ctx), compile[M](y, ctx)).mapN {
                (sg, compiledX, compiledY) =>
                  addEvo(compiledX.asExprF, compiledY.asExprF)(sg)
              }
            case tpe =>
              (M.fromEither(Type.group(tpe)), compile[M](x, ctx), compile[M](y, ctx)).mapN {
                (sg, compiledX, compiledY) =>
                  expressionModule.Add(compiledX.asExpr, compiledY.asExpr)(sg)
              }
          }

        case App2(PredefinedConstant.Div, x, y) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            expressionModule.Div(compiledX.asExpr, compiledY.asExpr)
          }

        case App2(PredefinedConstant.Exp, x, y) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            expressionModule.Exp(compiledX.asExpr, compiledY.asExpr)
          }

        case App(PredefinedConstant.Inverse, x) =>
          x.tpe match {
            // Overload - for evolutions
            case Type.Evo(tpe) =>
              (M.fromEither(Type.group(tpe)), compile[M](x, ctx)).mapN { (group, compiledX) =>
                inverseEvo(compiledX.asExprF)(group)
              }
            case tpe =>
              (M.fromEither(Type.group(tpe)), compile[M](x, ctx)).mapN { (g, compiledX) =>
                expressionModule.Inverse(compiledX.asExpr)(g)
              }
          }

        case App2(PredefinedConstant.Multiply, x, y) =>
          (M.fromEither(Type.vectorSpace(y.tpe)), compile[M](x, ctx), compile[M](y, ctx)).mapN {
            (vs, compiledX, compiledY) =>
              expressionModule.Multiply(compiledX.asExpr, compiledY.asExpr)(vs)
          }

        case App(PredefinedConstant.Cos, x) =>
          compile[M](x, ctx).map(compiledX => expressionModule.Cos(compiledX.asExpr))

        case App(PredefinedConstant.Sin, x) =>
          compile[M](x, ctx).map(compiledX => expressionModule.Sin(compiledX.asExpr))

        case App2(PredefinedConstant.Mod, x, y) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (cx, cy) =>
            expressionModule.Mod(cx.asExpr, cy.asExpr)
          }

        case App2(PredefinedConstant.Eq, x, y) =>
          for {
            compiledX <- compile[M](x, ctx)
            compiledY <- compile[M](y, ctx)
            eqTypeClass <- M.fromEither(Type.eqTypeClass(x.tpe))
          } yield expressionModule.Equals[x.Out](compiledX, compiledY.asExpr)(eqTypeClass)

        case App3(PredefinedConstant.If, x, y, z) =>
          (compile[M](x, ctx), compile[M](y, ctx), compile[M](z, ctx)).mapN { (compiledX, compiledY, compiledZ) =>
            expressionModule.IfThen(compiledX.asExpr, compiledY, compiledZ.asExpr)
          }

        case App2(PredefinedConstant.Cartesian, x, y) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            cartesian(compiledX.asExprF, compiledY.asExprF)
          }
        case App2(PredefinedConstant.Polar, x, y) =>
          (compile[M](x, ctx), compile[M](y, ctx)).mapN { (compiledX, compiledY) =>
            polar(compiledX.asExprF, compiledY.asExprF)
          }
        case App(PredefinedConstant.Constant, x) =>
          compile[M](x, ctx).map(compiledX => constant(compiledX.asExpr))

        case App2(PredefinedConstant.Integrate, x, y) =>
          for {
            compiledX <- compile[M](x, ctx)
            compiledY <- compile[M](y, ctx)
            vs <- M.fromEither(Type.vectorSpace(x.tpe))
          } yield integrate(compiledX, compiledY.asExprF)(vs)

        case App2(PredefinedConstant.Solve1, x, y) =>
          for {
            compiledX <- compile[M](x, ctx)
            compiledY <- compile[M](y, ctx)
            vs <- M.fromEither(Type.vectorSpace(y.tpe))
          } yield solve1[y.Out](compiledX.asExprF[y.Out => y.Out], compiledY.asExpr)(vs)

        case App3(PredefinedConstant.Solve2, x, y, z) =>
          for {
            compiledX <- compile[M](x, ctx)
            compiledY <- compile[M](y, ctx)
            compiledZ <- compile[M](z, ctx)
            vs <- M.fromEither(Type.vectorSpace(y.tpe))
          } yield
            solve2[y.Out](
              compiledX.asExprF[y.Out => y.Out => y.Out],
              compiledY.asExpr[y.Out],
              compiledY.asExpr[y.Out]
            )(vs)

        case App2(PredefinedConstant.Concat, x, y) => // TODO gen new vars
          (M.fromEither(Type.unwrapF(x.tpe)), compile[M](x, ctx), compile[M](y, ctx)).mapN {
            (innerType, compiledX, compiledY) =>
              concat(
                compiledX.asExprF,
                compiledY.asExprF
              )
          }
        case App2(PredefinedConstant.Map, x, f) =>
          (compile[M](x, ctx), compile[M](f, ctx)).mapN { (compiledX, compiledF) =>
            map(
              compiledX.asExprF,
              compiledF.asExpr[Any => Any]
            )
          }
        case App2(PredefinedConstant.FlatMap, x, f) =>
          (compile[M](x, ctx), compile[M](f, ctx)).mapN { (compiledX, compiledF) =>
            flatMap(
              compiledX.asExprF,
              compiledF.asExpr[Any => F[Any]]
            )
          }
        case App2(PredefinedConstant.Take, n, e) =>
          (compile[M](n, ctx), compile[M](e, ctx)).mapN { (compiledN, compiledF) =>
            take(compiledN.asExpr, compiledF.asExprF)
          }

        case App3(PredefinedConstant.ZipWith, a, b, c) =>
          (compile[M](a, ctx), compile[M](b, ctx), compile[M](c, ctx)).mapN { (compiledA, compiledB, compiledF) =>
            zipWith(compiledA.asExprF, compiledB.asExprF, compiledF.asExpr[Any => Any => Any])
          }

        case App2(PredefinedConstant.Uniform, from, to) =>
          (compile[M](from, ctx), compile[M](to, ctx)).mapN { (compiledFrom, compiledTo) =>
            expressionModule.Uniform(compiledFrom.asExpr, compiledTo.asExpr)
          }
        case App3(PredefinedConstant.UniformDiscrete, from, to, step) =>
          (compile[M](from, ctx), compile[M](to, ctx), compile[M](step, ctx)).mapN {
            (compiledFrom, compiledTo, compiledStep) =>
              expressionModule.UniformDiscrete(
                compiledFrom.asExpr,
                compiledTo.asExpr,
                compiledStep.asExpr
              )
          }
        case App(PredefinedConstant.UniformChoice, choices) =>
          M.raiseError("We need to fix UniformChoice")
//          choices.traverse(choice => compile[M](choice, ctx).asInstanceOf[M[Any]]).map { compiledChoices =>
//            expressionModule.UniformChoice(compiledChoices.asInstanceOf[List[Expr[Any]]])
//          }

        case AST.App(f, x, _) =>
          (compile[M](f, ctx), compile[M](x, ctx)).mapN { (compiledF, compiledX) =>
            expressionModule.App(compiledF.asExpr[Any => Any], compiledX.asExpr[Any])
          }

        case _ =>
          M.raiseError(s"Invalid AST for expression $expr")
      }
    }.asInstanceOf[M[Expr[expr.Out]]]

    object App {
      def unapply(arg: AST): Option[(PredefinedConstant, AST)] = arg match {
        case AST.App(AST.Const(c, _), x, _) => Some((c, x))
        case _                              => None
      }
    }

    object App2 {
      def unapply(arg: AST): Option[(PredefinedConstant, AST, AST)] = arg match {
        case AST.App(App(c, x), y, _) => Some((c, x, y))
        case _                        => None
      }
    }

    object App3 {
      def unapply(arg: AST): Option[(PredefinedConstant, AST, AST, AST)] = arg match {
        case AST.App(App2(c, x, y), z, _) => Some((c, x, y, z))
        case _                            => None
      }
    }

    implicit class CastingOps(value: Any) {
      def asExpr[T]: Expr[T] = value.asInstanceOf[Expr[T]]
      def asExprF[T]: Expr[F[T]] = value.asInstanceOf[Expr[F[T]]]
    }
  }

  class VarContext(vars: List[String]) {
    def indexOf(variable: String): Int = vars.indexOf(variable)
    def push(variable: String): VarContext = new VarContext(variable :: vars)
  }

  object VarContext {
    val empty: VarContext = new VarContext(List.empty)
  }
}
