package evolution.compiler
import evolution.compiler.phases.ModuleCompiler
import evolution.compiler.module.Module
import evolution.compiler.types.TypeBinding
import evolution.compiler.types.TypeClasses.Qualified
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.types.Type
import evolution.logging.NoOpLogger
import evolution.compiler.phases.parser.FastParseParser
import evolution.compiler.phases.typer.UnificationTyper
import evolution.compiler.phases.compiler.DefaultCompiler

class ModuleCompilerSpec extends LanguageSpec {
  "Module compiler" - {
    "should extract type bindings" in {
      val compiler = new ModuleCompiler(FastParseParser, new UnificationTyper(NoOpLogger), DefaultCompiler, NoOpLogger)
      val code = "blah(x, y) = point(x, y) in line(x, y) = point(x, y) in export"
      val module = compiler.compile(code, initialModule)

      val inferredBinding = module.unsafeEvaluate.typeBindings.getBinding("line")

      val expectedBinding = TypeBinding("line", Qualified(Type.Double =>: Type.Double =>: Type.Point), false)
      inferredBinding should contain(expectedBinding)
    }
  }

  private lazy val initialModule = Module(
    TypingConfig.constantQualifiedTypes,
    identity
  )
}
