package evolution.primitive
import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait CompilerSpecModule[F[_]]
    extends FreeSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with CompilerModule[F]
    with TyperModule[F]
    with ParsersModule[F]
    with ASTArbitraries[F]
    with ASTModule[F] {}
