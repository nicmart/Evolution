package evolution.primitive
import evolution.geometry
import evolution.primitive.algebra.evolution.Evolution
import cats.implicits._

trait CompilerModule[F[_]] { self: WithAst[F] =>

  object Compiler {
    import ast._
    import PredefinedFunction._
    def compile[R[_]](expr: Expr, alg: Evolution[F, R], ctx: VarContext): Either[String, R[expr.Out]] = {
      def compile(expr: Expr, ctx: VarContext): Either[String, R[expr.Out]] = {
        expr match {
          case Expr.Var(name, tpe) =>
            println(s"VAR($name) ${ctx.indexOf(name)}")
            Right(alg.bind.varN[expr.Out](name, ctx.indexOf(name)))
          case fc @ Expr.FuncCall(funcId, args, tpe) =>
            compileFuncCall(fc, ctx)
          case Expr.Lambda(varName, body, tpe) =>
            compile(body, ctx.push(varName.name)).map(alg.bind.lambda(varName.name, _))
          case Expr.Let(varName, value, in, tpe) =>
            for {
              compiledValue <- compile(value, ctx)
              compiledIn <- compile(in, ctx.push(varName.name))
            } yield alg.bind.let(varName.name, compiledValue, compiledIn)
          case Expr.Number(n, Type.Dbl) =>
            Right(alg.constants.double(n.toDouble))
          case Expr.Number(n, Type.Integer) =>
            Right(alg.constants.double(n.toInt))
          case _ =>
            Left(s"Invalid type for expression $expr")
        }
      }.asInstanceOf[Either[String, R[expr.Out]]]

      def compileFuncCall(func: Expr.FuncCall, ctx: VarContext): Either[String, R[func.Out]] = {
        (func.funcId, func.args) match {
          case (Point, x :: y :: Nil) =>
            (compile(x, ctx), compile(y, ctx)).mapN { (compiledX, compiledY) =>
              alg.constants.point(compiledX.asInstanceOf[R[Double]], compiledY.asInstanceOf[R[Double]])
            }
          case (X, p :: Nil) =>
            compile(p, ctx).map { compiledP =>
              alg.constants.x(compiledP.asInstanceOf[R[geometry.Point]])
            }
          case (Y, p :: Nil) =>
            compile(p, ctx).map { compiledP =>
              alg.constants.y(compiledP.asInstanceOf[R[geometry.Point]])
            }
          case (Add, x :: y :: Nil) =>
            (Type.group(func.tpe), compile(x, ctx), compile(y, ctx)).mapN { (sg, compiledX, compiledY) =>
              alg.constants.add(compiledX.asInstanceOf[R[func.Out]], compiledY.asInstanceOf[R[func.Out]])(sg)
            }
          case (Inverse, x :: Nil) =>
            (Type.group(func.tpe), compile(x, ctx)).mapN { (g, compiledX) =>
              alg.constants.inverse(compiledX.asInstanceOf[R[func.Out]])(g)
            }
          case (Multiply, x :: y :: Nil) =>
            (Type.vectorSpace(func.tpe), compile(x, ctx), compile(y, ctx)).mapN { (vs, compiledX, compiledY) =>
              alg.constants.multiply(compiledX.asInstanceOf[R[Double]], compiledY.asInstanceOf[R[func.Out]])(vs)
            }
          case (Cos, x :: Nil) =>
            compile(x, ctx).map(compiledX => alg.constants.cos(compiledX.asInstanceOf[R[Double]]))
          case (Sin, x :: Nil) =>
            compile(x, ctx).map(compiledX => alg.constants.sin(compiledX.asInstanceOf[R[Double]]))
          case (Eq, x :: y :: Nil) =>
            for {
              compiledX <- compile(x, ctx)
              compiledY <- compile(y, ctx)
              eqTypeClass <- Type.eqTypeClass(x.tpe)
            } yield alg.constants.eq[x.Out](compiledX, compiledY.asInstanceOf[R[x.Out]])(eqTypeClass)
          case (If, x :: y :: z :: Nil) =>
            (compile(x, ctx), compile(y, ctx), compile(z, ctx)).mapN { (compiledX, compiledY, compiledZ) =>
              alg.constants.ifThen(compiledX.asInstanceOf[R[Boolean]], compiledY, compiledZ.asInstanceOf[R[y.Out]])
            }
          case (Fix, f :: Nil) =>
            compile(f, ctx).map { compiledF =>
              alg.bind.fix(compiledF.asInstanceOf[R[func.Out => func.Out]])
            }
          case (App, f :: x :: Nil) =>
            (compile(f, ctx), compile(x, ctx)).mapN { (compiledF, compiledX) =>
              alg.bind.app(compiledF.asInstanceOf[R[x.Out => func.Out]], compiledX.asInstanceOf[R[x.Out]])
            }
          case (Empty, Nil) =>
            Right(alg.chain.empty)
          case (Cons, x :: y :: Nil) => // TODO I am not sure if we can assume transitivity and remove redundant constraints
            (compile(x, ctx), compile(y, ctx)).mapN { (compiledX, compiledY) =>
              alg.chain.cons[x.Out](compiledX.asInstanceOf[R[x.Out]], compiledY.asInstanceOf[R[F[x.Out]]])
            }
          case (MapEmpty, x :: y :: Nil) =>
            (compile(x, ctx), compile(y, ctx)).mapN { (compiledX, compiledY) =>
              alg.chain
                .mapEmpty[func.Out](compiledX.asInstanceOf[R[F[func.Out]]], compiledY.asInstanceOf[R[F[func.Out]]])
            }
          case (MapCons, x :: f :: Nil) =>
            (compile(x, ctx), compile(f, ctx)).mapN { (compiledX, compiledF) =>
              alg.chain.mapCons[x.Out, func.Out](compiledX.asInstanceOf[R[F[x.Out]]])(
                compiledF.asInstanceOf[R[x.Out => F[x.Out] => F[func.Out]]]
              )
            }
          case (Cartesian, x :: y :: Nil) =>
            (compile(x, ctx), compile(y, ctx)).mapN { (compiledX, compiledY) =>
              alg.derived.cartesian(compiledX.asInstanceOf[R[F[Double]]], compiledY.asInstanceOf[R[F[Double]]])
            }
          case (Polar, x :: y :: Nil) =>
            (compile(x, ctx), compile(y, ctx)).mapN { (compiledX, compiledY) =>
              alg.derived.polar(compiledX.asInstanceOf[R[F[Double]]], compiledY.asInstanceOf[R[F[Double]]])
            }
          case (Constant, x :: Nil) =>
            compile(x, ctx).map(compiledX => alg.derived.constant(compiledX.asInstanceOf[R[func.Out]]))
          case (Integrate, x :: y :: Nil) =>
            for {
              compiledX <- compile(x, ctx)
              compiledY <- compile(y, ctx)
              vs <- Type.vectorSpace(x.tpe)
            } yield alg.derived.integrate[x.Out](compiledX, compiledY.asInstanceOf[R[F[x.Out]]])(vs)
          case (Solve1, x :: y :: Nil) =>
            for {
              compiledX <- compile(x, ctx)
              compiledY <- compile(y, ctx)
              vs <- Type.vectorSpace(y.tpe)
            } yield
              alg.derived
                .solve1[y.Out](compiledX.asInstanceOf[R[F[y.Out => y.Out]]], compiledY.asInstanceOf[R[y.Out]])(vs)
          case (Solve2, x :: y :: z :: Nil) =>
            for {
              compiledX <- compile(x, ctx)
              compiledY <- compile(y, ctx)
              compiledZ <- compile(z, ctx)
              vs <- Type.vectorSpace(y.tpe)
            } yield
              alg.derived.solve2[y.Out](
                compiledX.asInstanceOf[R[F[y.Out => y.Out => y.Out]]],
                compiledY.asInstanceOf[R[y.Out]],
                compiledY.asInstanceOf[R[y.Out]]
              )(vs)
          case (Concat, x :: y :: Nil) => // TODO gen new vars
            (Type.unwrapF(func.tpe), compile(x, ctx), compile(y, ctx)).mapN { (innerType, compiledX, compiledY) =>
              alg.derived.concat[innerType.Out](
                compiledX.asInstanceOf[R[F[innerType.Out]]],
                compiledY.asInstanceOf[R[F[innerType.Out]]]
              )
            }
          case (Map, x :: f :: Nil) =>
            (compile(x, ctx), compile(f, ctx)).mapN { (compiledX, compiledF) =>
              alg.derived
                .map[x.Out, func.Out](compiledX.asInstanceOf[R[F[x.Out]]], compiledF.asInstanceOf[R[x.Out => func.Out]])
            }
          case (FlatMap, x :: f :: Nil) =>
            (Type.unwrapF(func.tpe), compile(x, ctx), compile(f, ctx)).mapN { (innerType, compiledX, compiledF) =>
              alg.derived.flatMap[x.Out, innerType.Out](
                compiledX.asInstanceOf[R[F[x.Out]]],
                compiledF.asInstanceOf[R[x.Out => F[innerType.Out]]]
              )
            }
          case (Take, n :: e :: Nil) =>
            (Type.unwrapF(func.tpe), compile(n, ctx), compile(e, ctx)).mapN { (innerType, compiledN, compiledF) =>
              alg.derived.take(compiledN.asInstanceOf[R[Int]], compiledF.asInstanceOf[R[F[innerType.Out]]])
            }
          case (Uniform, from :: to :: Nil) =>
            (compile(from, ctx), compile(to, ctx)).mapN { (compiledFrom, compiledTo) =>
              alg.distribution.uniform(compiledFrom.asInstanceOf[R[Double]], compiledTo.asInstanceOf[R[Double]])
            }
          case _ => Left(s"Invalid type for expression $func")
        }
      }.asInstanceOf[Either[String, R[func.Out]]]

      compile(expr, ctx)
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
