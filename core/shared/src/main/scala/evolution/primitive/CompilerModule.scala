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
    import PredefinedConstant._
    def compile[M[_]](expr: AST, ctx: VarContext)(implicit M: MonadError[M, String]): M[Expr[expr.Out]] = {
      expr match {
        case AST.Var(name, tpe) =>
          VarN(ctx.indexOf(name), name).pure[M]
        case fc @ AST.Const(id, tpe) =>
          compileConstant[M](fc, ctx)
        case AST.Lambda(varName, body, tpe) =>
          compile[M](body, ctx.push(varName.name)).map(Lambda(varName.name, _))

        case App(PredefinedConstant.Fix, x) =>
          compile[M](x, ctx).map { compiledX =>
            expressionModule.Fix(compiledX.asInstanceOf[Expr[Any => Any]])
          }

        case App2(PredefinedConstant.Cons, head, tail) =>
          (compile[M](head, ctx), compile[M](tail, ctx)).mapN { (head, tail) =>
            expressionModule.Cons(head.asInstanceOf[Expr[Any]], tail.asInstanceOf[Expr[F[Any]]])
          }

        case App2(PredefinedConstant.MapEmpty, a, b) =>
          (compile[M](a, ctx), compile[M](b, ctx)).mapN { (compiledA, compiledB) =>
            expressionModule.MapEmpty(compiledA.asInstanceOf[Expr[F[Any]]], compiledB.asInstanceOf[Expr[F[Any]]])
          }

        case App2(PredefinedConstant.MapCons, a, f) =>
          (compile[M](a, ctx), compile[M](f, ctx)).mapN { (compiledA, compiledF) =>
            expressionModule.MapCons(
              compiledA.asInstanceOf[Expr[F[Any]]],
              compiledF.asInstanceOf[Expr[Any => F[Any] => F[Any]]]
            )
          }

        case AST.App(f, x, _) =>
          (compile[M](f, ctx), compile[M](x, ctx)).mapN { (compiledF, compiledX) =>
            expressionModule.App(compiledF.asInstanceOf[Expr[Any => Any]], compiledX.asInstanceOf[Expr[Any]])
          }
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

    def compileConstant[M[_]](const: AST.Const, ctx: VarContext)(
      implicit M: MonadError[M, String]
    ): M[Expr[const.Out]] = {
      const.id match {
        case Point => expressionModule.Pnt().pure[M]
        case X     => expressionModule.X().pure[M]
        case Y     => expressionModule.Y().pure[M]
        case Floor => expressionModule.Floor().pure[M]
        case Add =>
          const.tpe match {
            // Overload + for evolutions
            case Type.Arrow(Type.Evo(tpe), _) =>
              M.fromEither(Type.group(tpe)).map(addEvo(_))
            case Type.Arrow(tpe, _) =>
              M.fromEither(Type.group(tpe)).map { group =>
                expressionModule.Add()(group)
              }
          }

        case Div => expressionModule.Div().pure[M]
        case Exp => expressionModule.Exp().pure[M]
        case Inverse =>
          const.tpe match {
            // Overload - for evolutions
            case Type.Arrow(Type.Evo(tpe), _) =>
              M.fromEither(Type.group(tpe)).map { implicit group =>
                inverseEvo
              }
            case Type.Arrow(tpe, _) =>
              M.fromEither(Type.group(tpe)).map { implicit group =>
                expressionModule.Inverse()
              }
          }

        case Multiply =>
          const.tpe match {
            case Type.Arrow(_, Type.Arrow(tpe, _)) =>
              M.fromEither(Type.vectorSpace(tpe)).map { implicit vectorSpace =>
                expressionModule.Multiply()
              }
          }

        case Cos => expressionModule.Cos().pure[M]
        case Sin => expressionModule.Sin().pure[M]
        case PI  => expressionModule.Dbl(Math.PI).pure[M]
        case Mod => expressionModule.Mod().pure[M]
        case Eq =>
          const.tpe match {
            case Type.Arrow(tpe, _) =>
              M.fromEither(Type.eqTypeClass(tpe)).map { implicit eq =>
                expressionModule.Equals()
              }
            case _ => M.raiseError(s"Unexpected type for Eq: ${const.tpe}")
          }

        case If        => expressionModule.IfThen().pure[M]
        case Empty     => expressionModule.Empty().pure[M]
        case Cartesian => cartesian.pure[M]
        case Polar     => polar.pure[M]
        case Constant  => constant.pure[M]
        case Integrate =>
          const.tpe match {
            case Type.Arrow(from, _) => M.fromEither(Type.vectorSpace(from).map(implicit vs => integrate))
            case _                   => M.raiseError(s"Unexpected type for Integrate: ${const.tpe}")
          }
        case Solve1 =>
          const.tpe match {
            case Type.Arrow(_, Type.Arrow(t, _)) => M.fromEither(Type.vectorSpace(t).map(implicit cs => solve1))
            case _                               => M.raiseError(s"Unexpected type for Solve1: ${const.tpe}")
          }

        case Solve2 =>
          const.tpe match {
            case Type.Arrow(_, Type.Arrow(t, _)) => M.fromEither(Type.vectorSpace(t).map(implicit cs => solve2))
            case _                               => M.raiseError(s"Unexpected type for Solve2: ${const.tpe}")
          }
        case Concat          => concat.pure[M]
        case Map             => map.pure[M]
        case FlatMap         => flatMap.pure[M]
        case Take            => take.pure[M]
        case ZipWith         => zipWith.pure[M]
        case Uniform         => expressionModule.Uniform().pure[M]
        case UniformDiscrete => expressionModule.UniformDiscrete().pure[M]
        case UniformChoice   => expressionModule.UniformChoice().pure[M]
        case _               => M.raiseError(s"Invalid type for expression $const")
      }
    }.asInstanceOf[M[Expr[const.Out]]]

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
  }

  class VarContext(vars: List[String]) {
    def indexOf(variable: String): Int = vars.indexOf(variable)
    def push(variable: String): VarContext = new VarContext(variable :: vars)
  }

  object VarContext {
    val empty: VarContext = new VarContext(List.empty)
  }
}
