package evolution.compiler.phases

import evolution.compiler.LanguageSpec
import evolution.compiler.phases.parser.FastParseParser
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.phases.typer.{RecursiveTyper, model}
import evolution.compiler.term.{Definition, TreeToTermCompiler}
import evolution.compiler.types.Type
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.Qualified
import evolution.logging.NoOpLogger
import evolution.compiler.term.Term._

class ModuleCompilerSpec extends LanguageSpec {
  "Module compiler" - {
    "should extract definitions" in {
      val typer = new RecursiveTyper
      val compiler = new ModuleCompiler(FastParseParser, typer, new TreeToTermCompiler, NoOpLogger)
      val code = "blah(x, y) = point(x, y) in line(x, y) = point(x, y) in export"
      val module = compiler.compile(code, initialModule)

      val compileDefinition = module.unsafeRight.findDefinition("line")

      val line = Lambda("x", Lambda("y", Apply(Apply(Id("point"), Id("x")), Id("y"))))
      val expectedDefinition =
        Definition("line", line, Qualified(Scheme(Type.Double =>: Type.Double =>: Type.Point)))
      compileDefinition should contain(expectedDefinition)
    }
  }

  private lazy val initialModule = TypingConfig.constantsModule
}
