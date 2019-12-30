package evolution.compiler.phases

import evolution.compiler.LanguageSpec
import evolution.compiler.phases.parser.FastParseParser
import evolution.compiler.phases.typer.RecursiveTyper
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.term.Term._
import evolution.compiler.term.TreeToTermCompiler
import evolution.compiler.types.Type
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.Qualified
import evolution.logging.NoOpLogger

class ModuleCompilerSpec extends LanguageSpec {
  "Module compiler" - {
    "should extract definitions" in {
      val typer = new RecursiveTyper
      val compiler = new ModuleCompiler(FastParseParser, typer, new TreeToTermCompiler, NoOpLogger)
      val code = "blah(x, y) = point(x, y) in line(x, y) = point(x, y) in export"
      val module = compiler.compile(code, initialModule)

      val compileDefinition = module.unsafeRight.findDefinition("line").get
      compileDefinition.name shouldBe "line"
      compileDefinition.tpe shouldBe Qualified(Scheme(Type.Double =>: Type.Double =>: Type.Point))
      val Some(Lambda("x'", Lambda("y'", Apply(Apply(Value(f), Id("x'")), Id("y'"))))) = compileDefinition.term
    }
  }

  private lazy val initialModule = TypingConfig.constantsModule
}
