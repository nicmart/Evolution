package evolution.language

import cats.MonadError
import cats.data.Kleisli
import cats.implicits._
import evolution.data.ExpressionModule
import evolution.geometry.Point

// TODO Random extensions and self types, please to do something better
trait CompilerModule[F[_]] {
  self: DesugarModule[F]
    with ExpressionModule[F]
    with ASTModule[F]
    with TypesModule[F]
    with PredefinedConstantsModule[F] =>

  import Desugarer._
  import Expr._
  import TypeClasses._

  type Result[M[_], T] = Kleisli[M, VarContext, T]

  object Compiler {
    val Const = Constant

    def compile[M[_]](expr: AST)(implicit M: MonadError[M, String]): Result[M, Expr[expr.Out]] = {
      type K[T] = Result[M, T]
      val K = MonadError[K, String]

      def withVar[A](name: String)(ka: K[A]): K[A] = Kleisli.local[M, A, VarContext](_.push(name))(ka)
      def varContext: K[VarContext] = Kleisli((ctx: VarContext) => ctx.pure[M])

      expr match {
        case AST.Identifier(name, tpe, false) =>
          varContext.flatMap[Expr[expr.tpe.t.Out]] { ctx =>
            if (ctx.has(name)) (Var[expr.Out](name): Expr[expr.Out]).pure[K]
            else K.raiseError(s"Variable $name is not defined for identifier $expr")
          }

        case AST.Identifier(Const(Const.PI), _, _) =>
          Dbl(Math.PI).pure[K]

        case AST.Identifier(Const(Const.Empty), _, _) =>
          Empty().pure[K]

        case fc @ AST.Identifier(Const(id), _, _) =>
          s"Constant $id is not supported as first class value".raiseError[K, Expr[Any]]

        case AST.Lambda(varName, body, tpe) =>
          withVar(varName)(compile[M](body)).map(Lambda(varName, _))

        case AST.Let(varName, value, in, tpe) =>
          (compile[M](value), withVar(varName)(compile[M](in))).mapN { (compiledValue, compiledIn) =>
            Let(varName, compiledValue, compiledIn)
          }

        case AST.Number(n, Qualified(_, Type.Integer)) =>
          Integer(n.toInt).pure[K]

        case AST.Number(n, _) => // Default to Double for numeric literals
          Dbl(n.toDouble).pure[K]

        case AST.Bool(b, _) =>
          Bool(b).pure[K]

        case AST.App(AST.Identifier(Const(c), _, true), x, _) =>
          c match {
            case Const.Fix =>
              compile[M](x).map { compiledX =>
                Fix(compiledX.asExpr[Any => Any])
              }
            case Const.X =>
              compile[M](x).map { compiledP =>
                X(compiledP.asExpr)
              }
            case Const.Y =>
              compile[M](x).map { compiledP =>
                Y(compiledP.asExpr)
              }
            case Const.Floor =>
              compile[M](x).map(compiledD => Floor(compiledD.asExpr))

            case Const.ToDbl =>
              compile[M](x).map(compiledN => ToDbl(compiledN.asExpr))

            case Const.Abs =>
              compile[M](x).map(compiledX => Abs(compiledX.asExpr))

            case Const.Sign =>
              compile[M](x).map(compiledX => Sign(compiledX.asExpr))

            case Const.Inverse =>
              x.tpe.t match {
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

            case Const.Cos =>
              compile[M](x).map(compiledX => Cos(compiledX.asExpr))

            case Const.Sin =>
              compile[M](x).map(compiledX => Sin(compiledX.asExpr))

            case Const.Constant =>
              compile[M](x).map(compiledX => constant(compiledX.asExpr))

            case Const.Lift =>
              compile[M](x).map(x => constant(x.asExpr))

            case Const.Not =>
              compile[M](x).map { compiledA =>
                Not(compiledA.asExpr)
              }

          }

        case AST.App(
            AST.App(AST.App(AST.Identifier(Const(Constant.Lift), _, true), AST.Identifier(Const(c), _, true), _), x, _),
            y,
            _) =>
          c match {
            case Const.Point =>
              (x, y).compileN[M] { (cx, cy) =>
                liftedPoint(cx.asExprF, cy.asExprF)
              }

            case Const.Polar =>
              (x, y).compileN[M] { (cx, cy) =>
                liftedPolar(cx.asExprF, cy.asExprF)
              }

            case Const.Add =>
              for {
                tpe <- K.fromEither(Type.unwrapF(x.tpe.t))
                sg <- K.fromEither(Type.group(tpe))
                cx <- compile[M](x)
                cy <- compile[M](y)
              } yield liftedAdd(cx.asExprF, cy.asExprF)(sg).asExprF

            case Const.Multiply =>
              for {
                tpe <- K.fromEither(Type.unwrapF(y.tpe.t))
                vs <- K.fromEither(Type.vectorSpace(tpe))
                cx <- compile[M](x)
                cy <- compile[M](y)
              } yield liftedMult(cx.asExprF, cy.asExprF)(vs).asExprF

          }

        // App 2
        case AST.App(AST.App(AST.Identifier(Const(c), _, true), x, _), y, _) =>
          c match {
            case Const.Cons =>
              (x, y).compileN[M] { (head, tail) =>
                Cons(head.asExpr, tail.asExprF)
              }

            case Const.MapEmpty =>
              (x, y).compileN[M] { (compiledA, compiledB) =>
                MapEmpty(compiledA.asExprF, compiledB.asExprF)
              }

            case Const.MapCons =>
              (x, y).compileN[M] { (compiledA, compiledF) =>
                MapCons(
                  compiledA.asExprF,
                  compiledF.asExpr[Any => F[Any] => F[Any]]
                )
              }

            case Const.Point =>
              (x, y).compileN[M] { (compiledX, compiledY) =>
                Pnt(compiledX.asExpr, compiledY.asExpr)
              }

            case Const.Add =>
              (K.fromEither(Type.group(x.tpe.t)), compile[M](x), compile[M](y)).mapN { (sg, compiledX, compiledY) =>
                Add(compiledX.asExpr, compiledY.asExpr)(sg)
              }

            case Const.Minus =>
              (K.fromEither(Type.group(x.tpe.t)), compile[M](x), compile[M](y)).mapN { (group, compiledX, compiledY) =>
                minus(compiledX.asExpr, compiledY.asExpr)(group)
              }

            case Const.Div =>
              (x, y).compileN[M] { (compiledX, compiledY) =>
                Div(compiledX.asExpr, compiledY.asExpr)
              }

            case Const.Exp =>
              (x, y).compileN[M] { (compiledX, compiledY) =>
                Exp(compiledX.asExpr, compiledY.asExpr)
              }

            case Const.Multiply =>
              (K.fromEither(Type.vectorSpace(y.tpe.t)), compile[M](x), compile[M](y)).mapN {
                (vs, compiledX, compiledY) =>
                  Multiply(compiledX.asExpr, compiledY.asExpr)(vs)
              }

            case Const.Mod =>
              (x, y).compileN[M] { (cx, cy) =>
                Mod(cx.asExpr, cy.asExpr)
              }

            case Const.Eq =>
              for {
                compiledX <- compile[M](x)
                compiledY <- compile[M](y)
                eqTypeClass <- K.fromEither(Type.eqTypeClass(x.tpe.t))
              } yield Equals[x.Out](compiledX, compiledY.asExpr)(eqTypeClass)

            case Const.Neq =>
              for {
                compiledX <- compile[M](x)
                compiledY <- compile[M](y)
                eqTypeClass <- K.fromEither(Type.eqTypeClass(x.tpe.t))
              } yield Neq[x.Out](compiledX, compiledY.asExpr)(eqTypeClass)

            case Const.GreaterThan =>
              for {
                compiledA <- compile[M](x)
                compiledB <- compile[M](y)
                order <- K.fromEither(Type.order(x.tpe.t))
              } yield GreaterThan[x.Out](compiledA, compiledB.asExpr)(order)

            case Const.GreaterThanOrEqual =>
              for {
                compiledA <- compile[M](x)
                compiledB <- compile[M](y)
                ordering <- K.fromEither(Type.order(x.tpe.t))
              } yield GreaterThanOrEqual[x.Out](compiledA, compiledB.asExpr)(ordering)

            case Const.LessThan =>
              for {
                compiledA <- compile[M](x)
                compiledB <- compile[M](y)
                ordering <- K.fromEither(Type.order(x.tpe.t))
              } yield LessThan[x.Out](compiledA, compiledB.asExpr)(ordering)

            case Const.LessThanOrEqual =>
              for {
                compiledA <- compile[M](x)
                compiledB <- compile[M](y)
                order <- K.fromEither(Type.order(x.tpe.t))
              } yield LessThanOrEqual[x.Out](compiledA, compiledB.asExpr)(order)

            case Const.And =>
              (x, y).compileN[M] { (compiledA, compiledB) =>
                And(compiledA.asExpr, compiledB.asExpr)
              }

            case Const.Or =>
              (x, y).compileN[M] { (compiledA, compiledB) =>
                Or(compiledA.asExpr, compiledB.asExpr)
              }
            case Const.Polar =>
              (x, y).compileN[M] { (compiledX, compiledY) =>
                polar(compiledX.asExpr, compiledY.asExpr)
              }

            case Const.Integrate =>
              for {
                compiledX <- compile[M](x)
                compiledY <- compile[M](y)
                vs <- K.fromEither(Type.vectorSpace(x.tpe.t))
              } yield integrate(compiledX, compiledY.asExprF)(vs)

            case Const.Solve1 =>
              for {
                compiledX <- compile[M](x)
                compiledY <- compile[M](y)
                vs <- K.fromEither(Type.vectorSpace(y.tpe.t))
              } yield solve1[y.Out](compiledX.asExprF[y.Out => y.Out], compiledY.asExpr)(vs)

            case Const.Concat =>
              K.fromEither(Type.unwrapF(x.tpe.t)).flatMap { innerType =>
                (x, y).compileN[M] { (cx, cy) =>
                  concat(cx.asExprF, cy.asExprF)
                }
              }

            case Const.Map =>
              (x, y).compileN[M] { (compiledX, compiledF) =>
                map(
                  compiledX.asExprF,
                  compiledF.asExpr[Any => Any]
                )
              }

            case Const.FlatMap =>
              (x, y).compileN[M] { (compiledX, compiledF) =>
                flatMap(
                  compiledX.asExprF,
                  compiledF.asExpr[Any => F[Any]]
                )
              }

            case Const.Take =>
              (x, y).compileN[M] { (compiledN, compiledF) =>
                take(compiledN.asExpr, compiledF.asExprF)
              }
            case Const.While =>
              (x, y).compileN[M] { (compiledFa, compiledP) =>
                takeWhile(compiledFa.asExprF, compiledP.asExpr[Any => Boolean])
              }

            case Const.Until =>
              (x, y).compileN[M] { (compiledFa, compiledP) =>
                takeUntil(compiledFa.asExprF, compiledP.asExpr[Any => Boolean])
              }

            case Const.Uniform =>
              (x, y).compileN[M] { (compiledFrom, compiledTo) =>
                Uniform(compiledFrom.asExpr, compiledTo.asExpr)
              }

            case Const.UniformFrom =>
              (x, y).compileN[M] { (compiledN, compiledFt) =>
                UniformFrom(
                  compiledN.asExpr,
                  compiledFt.asExprF
                )
              }

            case Const.Normal =>
              (x, y).compileN[M] { (mu, sigma) =>
                Normal(mu.asExpr, sigma.asExpr)
              }
          }

        case AST.App(AST.App(AST.App(AST.Identifier(Const(c), _, true), x, _), y, _), z, _) =>
          c match {

            case Const.If =>
              (x, y, z).compileN[M] { (compiledX, compiledY, compiledZ) =>
                IfThen(compiledX.asExpr, compiledY, compiledZ.asExpr)
              }

            case Const.InRect =>
              (x, y, z).compileN[M] { (compiledTl, compiledBr, compiledP) =>
                InRect(compiledTl.asExpr[Point], compiledBr.asExpr[Point], compiledP.asExpr[Point])
              }

            case Const.Solve2 =>
              K.fromEither(Type.vectorSpace(y.tpe.t)).flatMap { vs =>
                (x, y, z).compileN[M] { (compiledX, compiledY, compiledZ) =>
                  solve2[y.Out](
                    compiledX.asExprF[y.Out => y.Out => y.Out],
                    compiledY.asExpr[y.Out],
                    compiledZ.asExpr[y.Out]
                  )(vs)
                }
              }

            case Const.ZipWith =>
              (x, y, z).compileN[M] { (compiledA, compiledB, compiledF) =>
                zipWith(compiledA.asExprF, compiledB.asExprF, compiledF.asExpr[Any => Any => Any])
              }

            case Const.UniformDiscrete =>
              (x, y, z).compileN[M] { (compiledFrom, compiledTo, compiledStep) =>
                UniformDiscrete(
                  compiledFrom.asExpr,
                  compiledTo.asExpr,
                  compiledStep.asExpr
                )
              }
          }

        case AST.App(f, x, _) =>
          (f, x).compileN[M] { (compiledF, compiledX) =>
            App(compiledF.asExpr[Any => Any], compiledX.asExpr[Any])
          }

        case _ =>
          M.raiseError(s"Invalid AST for expression $expr")
      }
    }.asInstanceOf[Result[M, Expr[expr.Out]]]

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
    def has(variable: String): Boolean = vars.contains(variable)
    def indexOf(variable: String): Int = vars.indexOf(variable)
    def push(variable: String): VarContext = new VarContext(variable :: vars)
  }

  object VarContext {
    val empty: VarContext = new VarContext(List.empty)
  }
}
