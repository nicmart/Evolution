package evolution.compiler.impl.evaluation

import evolution.compiler.expression.typeclass._
import evolution.compiler.phases.typer.config._
import evolution.compiler.types.{Type, Typed}
import evolution.geometry.Point
import evolution.materialization.Evolution

class ConstantMaterializer {
  def materialize(const: Constant, tpe: Type): Any = const match {
    case constant: Constant0 =>
      constant match {
        case Constant0.PI          => Math.PI
        case Constant0.Empty       => Evolution.empty
        case Constant0.Noise       => ???
        case Constant0.OctaveNoise => ???
      }
    case constant: Constant1 =>
      constant match {
        case plain: Constant1Plain =>
          plain match {
            case Constant1.X             => (p: Point) => p.x
            case Constant1.Y             => (p: Point) => p.y
            case Constant1.Floor         => (d: Double) => Math.floor(d)
            case Constant1.ToDbl         => (n: Int) => n.toDouble
            case Constant1.Abs           => ???
            case Constant1.Sign          => ???
            case Constant1.Norm          => ???
            case Constant1.Versor        => ???
            case Constant1.Sin           => ???
            case Constant1.Cos           => ???
            case Constant1.Not           => ???
            case Constant1.Constant      => (a: Any) => Evolution.constant(a)
            case Constant1.FromList      => ???
            case Constant1.Fix           => ???
            case Constant1.Flatten       => ???
            case Constant1.Parallel      => ???
            case Constant1.UniformChoice => ???
          }
        case Constant1.Inverse => ???
        case Constant1.Derive  => ???
      }
    case constant: Constant2 =>
      constant match {
        case plain: Constant2Plain =>
          plain match {
            case Constant2.Point       => ???
            case Constant2.LiftedPoint => ???
            case Constant2.Polar       => ???
            case Constant2.LiftedPolar => ???
            case Constant2.Div         => ???
            case Constant2.Exp         => ???
            case Constant2.Mod         => ???
            case Constant2.And         => ???
            case Constant2.Or          => ???
            case Constant2.Cons        => ???
            case Constant2.WithFirst   => ???
            case Constant2.Connect     => ???
            case Constant2.Roll        => ???
            case Constant2.Concat      => ???
            case Constant2.Map         => ???
            case Constant2.SlidingMap  => ???
            case Constant2.Iterate     => ???
            case Constant2.FlatMap     => ???
            case Constant2.Take        => ???
            case Constant2.Grouped     => ???
            case Constant2.Filter      => ???
            case Constant2.While       => ???
            case Constant2.Until       => ???
            case Constant2.Uniform     => ???
            case Constant2.UniformFrom => ???
            case Constant2.Normal      => ???
          }
        case Constant2.Multiply => // Type-classes - option 1: someone gives us the type-class
          (mul: Multiplicative[Any, Any, Any], a: Any, b: Any) => MaterializeMultiplication(mul)(a, b)

        case Constant2.Add => // Type-classes - option 2: we receive the types and we extract the type-class
          (a: Typed[Any], b: Typed[Any]) => // Note: this would be very slow, instance should be extracted outside
        //MaterializeAddition(TypingConfig.additive(a.tpe, b.tpe, tpe).right.get)(a.value, b.value)
        case Constant2.Minus              => ???
        case Constant2.Eq                 => ???
        case Constant2.Neq                => ???
        case Constant2.GreaterThan        => ???
        case Constant2.GreaterThanOrEqual => ???
        case Constant2.LessThan           => ???
        case Constant2.LessThanOrEqual    => ???
        case Constant2.Integrate          => ???
        case Constant2.Solve1             => ???
        case Constant2.MapWithDerivative  => ???
      }
    case constant: Constant3 =>
      constant match {
        case plain: Constant3Plain =>
          plain match {
            case Constant3.SmoothStep      => ???
            case Constant3.ZipWith         => ???
            case Constant3.Range           => ???
            case Constant3.Roll2           => ???
            case Constant3.Iterate2        => ???
            case Constant3.InRect          => ???
            case Constant3.If              => ???
            case Constant3.UniformDiscrete => ???
          }
        case Constant3.Solve2 => ???
      }
  }
}
