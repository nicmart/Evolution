package evolution.primitive

import cats.arrow.Arrow
import cats.data.Kleisli
import cats.{ MonadError, Traverse }
import cats.implicits._
import evolution.data.ExpressionModule
import evolution.geometry
import evolution.data.WithExpression

// TODO Random extensions and self types, please to do something better
trait CompilerModule[FF[_]] extends DesugarModule[FF] with WithExpression[FF] { self: WithAst[FF] =>

  import expressionModule._
  import Desugarer._

  type Result[M[_], T] = Kleisli[M, VarContext, T]

  object Compiler {
    import ast._
    val Const = PredefinedConstant

    def compile[M[_]](expr: AST)(implicit M: MonadError[M, String]): Result[M, Expr[expr.Out]] = {
      type K[T] = Result[M, T]
      val K = MonadError[K, String]

      def pushVar(name: String): K[VarContext] = Kleisli((ctx: VarContext) => ctx.push(name).pure[M])

      expr match {
        case AST.Var(name, tpe) =>
          Kleisli((ctx: VarContext) => VarN(ctx.indexOf(name), name).pure[M])

        case AST.Const(Const.PI, _) =>
          Dbl(Math.PI).pure[K]

        case AST.Const(Const.Empty, _) =>
          Empty().pure[K]

        case fc @ AST.Const(id, tpe) =>
          s"Constant $id is not supported as first class value".raiseError[K, Expr[Any]]

        case AST.Lambda(varName, body, tpe) =>
          (pushVar(varName.name) andThen compile[M](body)).map(Lambda(varName.name, _))

        case AST.Let(varName, value, in, tpe) =>
          (compile[M](value), pushVar(varName.name) andThen compile[M](in)).mapN { (compiledValue, compiledIn) =>
            Let(varName.name, compiledValue, compiledIn)
          }

        case AST.Number(n, Type.Integer) =>
          Integer(n.toInt).pure[K]

        case AST.Number(n, _) => // Default to Double for numeric literals
          Dbl(n.toDouble).pure[K]

        case App(Const.Fix, x) =>
          compile[M](x).map { compiledX =>
            Fix(compiledX.asExpr[Any => Any])
          }

        case App2(Const.Cons, head, tail) =>
          (head, tail).compileN[M] { (head, tail) =>
            Cons(head.asExpr, tail.asExprF)
          }

        case App2(Const.MapEmpty, a, b) =>
          (a, b).compileN[M] { (compiledA, compiledB) =>
            MapEmpty(compiledA.asExprF, compiledB.asExprF)
          }

        case App2(Const.MapCons, a, f) =>
          (a, f).compileN[M] { (compiledA, compiledF) =>
            MapCons(
              compiledA.asExprF,
              compiledF.asExpr[Any => F[Any] => F[Any]]
            )
          }

        case App2(Const.Point, x, y) =>
          (x, y).compileN[M] { (compiledX, compiledY) =>
            Pnt(compiledX.asExpr, compiledY.asExpr)
          }

        case LiftApp2(Const.Point, x, y) =>
          (x, y).compileN[M] { (cx, cy) =>
            liftedPoint(cx.asExprF, cy.asExprF)
          }

        case LiftApp2(Const.Polar, x, y) =>
          (x, y).compileN[M] { (cx, cy) =>
            liftedPolar(cx.asExprF, cy.asExprF)
          }

        case App(Const.X, p) =>
          compile[M](p).map { compiledP =>
            X(compiledP.asExpr)
          }
          
        case App(Const.Y, p) =>
          compile[M](p).map { compiledP =>
            Y(compiledP.asExpr)
          }
          
        case App(Const.Floor, d) =>
          compile[M](d).map(compiledD => Floor(compiledD.asExpr))

        case App2(Const.Add, x, y) =>
          (K.fromEither(Type.group(x.tpe)), compile[M](x), compile[M](y)).mapN { (sg, compiledX, compiledY) =>
            Add(compiledX.asExpr, compiledY.asExpr)(sg)
          }

        case LiftApp2(Const.Add, x, y) =>
          for {
            tpe <- K.fromEither(Type.unwrapF(x.tpe))
            sg <- K.fromEither(Type.group(tpe))
            cx <- compile[M](x)
            cy <- compile[M](y)
          } yield liftedAdd(cx.asExprF, cy.asExprF)(sg).asExprF

        case App2(Const.Div, x, y) =>
          (x, y).compileN[M] { (compiledX, compiledY) =>
            Div(compiledX.asExpr, compiledY.asExpr)
          }

        case App2(Const.Exp, x, y) =>
          (x, y).compileN[M] { (compiledX, compiledY) =>
            Exp(compiledX.asExpr, compiledY.asExpr)
          }

        // TODO this is not in-line with other liftings.
        case App(Const.Inverse, x) =>
          x.tpe match {
            // Overload - for evolutions
            case Type.Evo(tpe) =>
              (K.fromEither(Type.group(tpe)), compile[M](x)).mapN { (group, compiledX) =>
                inverseEvo(compiledX.asExprF)(group)
              }
            case tpe =>
              (K.fromEither(Type.group(tpe)), compile[M](x)).mapN { (g, compiledX) =>
                Inverse(compiledX.asExpr)(g)
              }
          }

        case App2(Const.Multiply, x, y) =>
          (K.fromEither(Type.vectorSpace(y.tpe)), compile[M](x), compile[M](y)).mapN { (vs, compiledX, compiledY) =>
            Multiply(compiledX.asExpr, compiledY.asExpr)(vs)
          }

        case LiftApp2(Const.Multiply, x, y) =>
          for {
            tpe <- K.fromEither(Type.unwrapF(y.tpe))
            vs <- K.fromEither(Type.vectorSpace(tpe))
            cx <- compile[M](x)
            cy <- compile[M](y)
          } yield liftedMult(cx.asExprF, cy.asExprF)(vs).asExprF

        case App(Const.Cos, x) =>
          compile[M](x).map(compiledX => Cos(compiledX.asExpr))

        case App(Const.Sin, x) =>
          compile[M](x).map(compiledX => Sin(compiledX.asExpr))

        case App2(Const.Mod, x, y) =>
          (x, y).compileN[M] { (cx, cy) =>
            Mod(cx.asExpr, cy.asExpr)
          }

        case App2(Const.Eq, x, y) =>
          for {
            compiledX <- compile[M](x)
            compiledY <- compile[M](y)
            eqTypeClass <- K.fromEither(Type.eqTypeClass(x.tpe))
          } yield Equals[x.Out](compiledX, compiledY.asExpr)(eqTypeClass)

        case App3(Const.If, x, y, z) =>
          (x, y, z).compileN[M] { (compiledX, compiledY, compiledZ) =>
            IfThen(compiledX.asExpr, compiledY, compiledZ.asExpr)
          }

        case App2(Const.Polar, x, y) =>
          (x, y).compileN[M] { (compiledX, compiledY) =>
            polar(compiledX.asExpr, compiledY.asExpr)
          }
        case App(Const.Constant, x) =>
          compile[M](x).map(compiledX => constant(compiledX.asExpr))

        case App2(Const.Integrate, x, y) =>
          for {
            compiledX <- compile[M](x)
            compiledY <- compile[M](y)
            vs <- K.fromEither(Type.vectorSpace(x.tpe))
          } yield integrate(compiledX, compiledY.asExprF)(vs)

        case App2(Const.Solve1, x, y) =>
          for {
            compiledX <- compile[M](x)
            compiledY <- compile[M](y)
            vs <- K.fromEither(Type.vectorSpace(y.tpe))
          } yield solve1[y.Out](compiledX.asExprF[y.Out => y.Out], compiledY.asExpr)(vs)

        case App3(Const.Solve2, x, y, z) =>
          K.fromEither(Type.vectorSpace(y.tpe)).flatMap { vs =>
            (x, y, z).compileN[M] { (compiledX, compiledY, compiledZ) =>
              solve2[y.Out](
                compiledX.asExprF[y.Out => y.Out => y.Out],
                compiledY.asExpr[y.Out],
                compiledZ.asExpr[y.Out]
              )(vs)
            }
          }

        case App2(Const.Concat, x, y) =>
          K.fromEither(Type.unwrapF(x.tpe)).flatMap { innerType =>
            (x, y).compileN[M] { (cx, cy) =>
              concat(cx.asExprF, cy.asExprF)
            }
          }

        case App2(Const.Map, x, f) =>
          (x, f).compileN[M] { (compiledX, compiledF) =>
            map(
              compiledX.asExprF,
              compiledF.asExpr[Any => Any]
            )
          }
          
        case App2(Const.FlatMap, x, f) =>
          (x, f).compileN[M] { (compiledX, compiledF) =>
            flatMap(
              compiledX.asExprF,
              compiledF.asExpr[Any => F[Any]]
            )
          }
          
        case App2(Const.Take, n, e) =>
          (n, e).compileN[M] { (compiledN, compiledF) =>
            take(compiledN.asExpr, compiledF.asExprF)
          }

        case App(Const.Lift, f) => compile[M](f).map(x => constant(x.asExpr))

        case App3(Const.ZipWith, a, b, c) =>
          (a, b, c).compileN[M] { (compiledA, compiledB, compiledF) =>
            zipWith(compiledA.asExprF, compiledB.asExprF, compiledF.asExpr[Any => Any => Any])
          }

        case App2(Const.Uniform, from, to) =>
          (from, to).compileN[M] { (compiledFrom, compiledTo) =>
            expressionModule.Uniform(compiledFrom.asExpr, compiledTo.asExpr)
          }
          
        case App3(Const.UniformDiscrete, from, to, step) =>
          (from, to, step).compileN[M] { (compiledFrom, compiledTo, compiledStep) =>
            expressionModule.UniformDiscrete(
              compiledFrom.asExpr,
              compiledTo.asExpr,
              compiledStep.asExpr
            )
          }
        case App(Const.UniformChoice, choices) =>
          M.raiseError("We need to fix UniformChoice")
//          choices.traverse(choice => compile[M](choice).asInstanceOf[M[Any]]).map { compiledChoices =>
//            expressionModule.UniformChoice(compiledChoices.asInstanceOf[List[Expr[Any]]])
//          }

        case AST.App(f, x, _) =>
          (f, x).compileN[M] { (compiledF, compiledX) =>
            expressionModule.App(compiledF.asExpr[Any => Any], compiledX.asExpr[Any])
          }

        case _ =>
          M.raiseError(s"Invalid AST for expression $expr")
      }
    }.asInstanceOf[Result[M, Expr[expr.Out]]]

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

    object LiftApp2 {
      def unapply(arg: AST): Option[(PredefinedConstant, AST, AST)] = arg match {
        case AST.App(AST.App(AST.App(AST.Const(Const.Lift, _), AST.Const(const, _), _), x, _), y, _) =>
          Some((const, x, y))
        case _ => None
      }
    }

    implicit class Tuple2Ops(ts: (AST, AST)) {
      def compileN[M[_]](f: (Expr[_], Expr[_]) => Expr[_])(implicit E: MonadError[M, String]): Result[M, Expr[_]] =
        (compile[M](ts._1), compile[M](ts._2)).mapN(f)
    }

    implicit class Tuple3Ops(ts: (AST, AST, AST)) {
      def compileN[M[_]](
        f: (Expr[_], Expr[_], Expr[_]) => Expr[_]
      )(implicit E: MonadError[M, String]): Result[M, Expr[_]] =
        (compile[M](ts._1), compile[M](ts._2), compile[M](ts._3)).mapN(f)
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
