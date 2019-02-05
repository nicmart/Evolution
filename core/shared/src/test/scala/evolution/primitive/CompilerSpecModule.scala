package evolution.primitive
import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait CompilerSpecModule[F[_]]
    extends FreeSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with InitialCompilerModule[F]
    with TyperModule[F]
    with ParsersModule[F]
    with ASTArbitraries[F]
    with WithAst[F] {}
