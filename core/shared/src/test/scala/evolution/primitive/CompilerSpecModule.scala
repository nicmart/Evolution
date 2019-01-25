package evolution.primitive
import evolution.data.initial
import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait CompilerSpecModule[F[_]]
    extends FreeSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with CompilerModule[initial.F]
    with TyperModule[initial.F]
    with ParsersModule[initial.F]
    with ASTArbitraries[initial.F]
    with WithAst[initial.F] {}
