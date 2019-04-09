package evolution.language
import InterpreterModule._
import evolution.data.{ Ctx, ExpressionModule }
import evolution.data.EvaluationContext._
import evolution.geometry.Point

sealed trait Evo[T]

trait ExperimentalInterpreterModule extends ExpressionModule[Evo] {
  import Expr._

  trait Interpreter[T1, T2] {
    def interpret(t1: Expr[T1]): Out[T2]
  }

  object Interpreter {
    def instance[T1, T2](f: Expr[T1] => Out[T2]): Interpreter[T1, T2] =
      (t1: Expr[T1]) => f(t1)

    implicit class Ops[T1](t1: Expr[T1]) {
      def interpret[T2](implicit interpreter: Interpreter[T1, T2]): Out[T2] =
        interpreter.interpret(t1)
    }

    implicit val double: Interpreter[Double, Double] = {
      case Dbl(d)    => Out.pure(d)
      case ToDbl(n)  => n.interpret[Int].map(_.toDouble)
      case Var(name) => Contextual.instance[Double](get[Any](_, name).asInstanceOf[Double])
    }

    implicit val integer: Interpreter[Int, Int] =
      Interpreter.instance[Int, Int] {
        case Integer(n) => Out.pure(n)
        case Floor(d)   => d.interpret[Double].map(_.toInt)
        case Var(name)  => Contextual.instance[Int](get[Any](_, name).asInstanceOf[Int])
      }

    implicit val point: Interpreter[Point, Point] =
      Interpreter.instance[Point, Point] {
        case Pnt(x, y) => Out.map2(x.interpret[Double], y.interpret[Double])(Point.apply)
      }
  }
}
