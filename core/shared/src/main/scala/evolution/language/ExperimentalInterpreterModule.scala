package evolution.language
import evolution.data.EvaluationContext._
import evolution.data.ExpressionModule
import evolution.geometry.Point
import evolution.language.InterpreterModule._
import evolution.materialization.RNGRepr

sealed trait Evo[T]

trait ExperimentalInterpreterModule extends ExpressionModule[Evo] {
  import Expr._

  trait Interpreter[T1, T2] {
    def interpret(t1: Expr[T1]): Out[T2]
  }

  object Interpreter {
    def apply[T1, T2](implicit int: Interpreter[T1, T2]): Interpreter[T1, T2] = int

    def instance[T1, T2](f: Expr[T1] => Out[T2]): Interpreter[T1, T2] =
      (t1: Expr[T1]) => f(t1)

    implicit class Ops[T1](t1: Expr[T1]) {
      def interpret[T2](implicit interpreter: Interpreter[T1, T2]): Out[T2] =
        interpreter.interpret(t1)
    }

    implicit class Ops2[A1, A2](pairOfExprs: (Expr[A1], Expr[A2])) {
      def interpret2[T1, T2, T3](
        f: (T1, T2) => T3
      )(implicit int1: Interpreter[A1, T1], int2: Interpreter[A2, T2]): Out[T3] =
        Out.map2(pairOfExprs._1.interpret[T1], pairOfExprs._2.interpret[T2])(f)
    }

    implicit lazy val double: Interpreter[Double, Double] =
      baseInterpreter[Double] {
        case Dbl(d)   => Out.pure(d)
        case ToDbl(n) => n.interpret.map(_.toDouble)
        case X(p)     => p.interpret.map(_.x)
        case Y(p)     => p.interpret.map(_.y)
      }(double)

    implicit lazy val integer: Interpreter[Int, Int] =
      baseInterpreter[Int] {
        case Integer(n) => Out.pure(n)
        case Floor(d)   => d.interpret.map(_.toInt)
      }(integer)

    implicit lazy val point: Interpreter[Point, Point] =
      baseInterpreter[Point] {
        case Pnt(x, y) => (x, y).interpret2(Point.apply)
      }(point)

    implicit def evolution[T1, T2](implicit inner: Interpreter[T1, T2]): Interpreter[Evo[T1], RNGRepr[T2]] =
      Interpreter.instance[Evo[T1], RNGRepr[T2]] {
        case Empty() => Out.pure(RNGRepr(rng => (rng, None)))
        case Cons(head, tail) =>
          val interpretedHead = head.interpret[T2]
          val interpretedTail = tail.interpret[RNGRepr[T2]]
          Contextual.instance { ctx =>
            val h = interpretedHead(ctx)
            RNGRepr { rng =>
              (rng, Some((h, interpretedTail(ctx))))
            }
          }

        case MapEmpty(ev1, ev2) =>
          (ev1, ev2).interpret2[RNGRepr[T2], RNGRepr[T2], RNGRepr[T2]] { (compiled1, compiled2) =>
            RNGRepr { rng =>
              val (rng2, next) = compiled1.run(rng)
              next match {
                case None => compiled2.run(rng2)
                case _    => (rng2, next)
              }
            }
          }

        case MapCons(eva, f) => ???

//          (eva, f).interpret2[RNGRepr[T2], RNGRepr[T2], RNGRepr[T2]] { (compiledEva, compiledF) =>
//            RNGRepr { rng =>
//              val (rng2, next) = compiledEva.run(rng)
//              next match {
//                case None            => (rng2, None)
//                case Some((a, eva2)) => compiledF(a)(eva2).run(rng2)
//              }
//            }
//          }
      }

    private def common[T](int: => Interpreter[T, T]): PartialFunction[Expr[T], Out[T]] = {
      case Var(name)       => Contextual.instance[T](get[Any](_, name).asInstanceOf[T])
      case add @ Add(a, b) => (a, b).interpret2(add.semigroup.combine)(int, int)
    }

    private def baseInterpreter[T](f: PartialFunction[Expr[T], Out[T]])(self: => Interpreter[T, T]): Interpreter[T, T] =
      Interpreter.instance[T, T](f orElse common[T](self))
  }
}
