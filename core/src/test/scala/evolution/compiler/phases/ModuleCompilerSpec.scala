package evolution.compiler
import evolution.compiler.phases.ModuleCompiler
import evolution.compiler.module.Module
import evolution.compiler.types.Assumption
import evolution.compiler.types.TypeClasses.Qualified
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.types.Type
import evolution.compiler.types.Type.Scheme
import evolution.logging.NoOpLogger
import evolution.compiler.phases.parser.FastParseParser
import evolution.compiler.phases.typer.UnificationTyper
import evolution.compiler.phases.compiler.DefaultCompiler

class ModuleCompilerSpec extends LanguageSpec {
  "Module compiler" - {
    "should extract assumptions" in {
      val compiler = new ModuleCompiler(FastParseParser, new UnificationTyper(NoOpLogger), DefaultCompiler, NoOpLogger)
      val code = "blah(x, y) = point(x, y) in line(x, y) = point(x, y) in export"
      val module = compiler.compile(code, initialModule)

      val inferredAssumption = module.unsafeRight.assumptions.get("line")

      val expectedAssumption = Assumption("line", Qualified(Scheme(Type.Double =>: Type.Double =>: Type.Point)), false)
      inferredAssumption should contain(expectedAssumption)
    }
  }

  private lazy val initialModule = Module(
    TypingConfig.constantQualifiedTypes,
    identity
  )
}
