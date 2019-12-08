package evolution.compiler
import evolution.compiler.phases.{ExprModule, ExprModuleCompiler}
import evolution.compiler.phases.compiler.DefaultCompiler
import evolution.compiler.phases.parser.FastParseParser
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.phases.typer.{RecursiveTyper, model}
import evolution.compiler.types.Type
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.Qualified
import evolution.logging.NoOpLogger

class ExprExprModuleCompilerSpec extends LanguageSpec {
  "Module compiler" - {
    "should extract assumptions" in {
      val typer = new RecursiveTyper
      val compiler = new ExprModuleCompiler(FastParseParser, typer, DefaultCompiler, NoOpLogger)
      val code = "blah(x, y) = point(x, y) in line(x, y) = point(x, y) in export"
      val module = compiler.compile(code, initialModule)

      val inferredAssumption = module.unsafeRight.assumptions.get("line")

      val expectedAssumption =
        model.Assumption("line", Qualified(Scheme(Type.Double =>: Type.Double =>: Type.Point)), false)
      inferredAssumption should contain(expectedAssumption)
    }
  }

  private lazy val initialModule = ExprModule(
    TypingConfig.constantQualifiedTypes,
    identity
  )
}
