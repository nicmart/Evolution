package evolution.language
import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait LanguageSpec[F[_]]
    extends FreeSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with CompilerModule[F]
    with PredefinedConstantsModule[F]
    with TyperModule[F]
    with TypesModule[F]
    with ParserModule[F]
    with ASTArbitraries[F]
    with ASTModule[F]
    with InstancesModule[F]
