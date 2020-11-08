package evolution.compiler.term

import evolution.compiler.LanguageSpec
import evolution.compiler.term.Term.Literal.{LitBool, LitDouble, LitInt}
import evolution.compiler.term.Term.{Lambda, _}

class CaptureAvoidingRenamerTest extends LanguageSpec {
  "leave simple literals unaltered" in {
    rename(Lit(LitInt(1))) shouldBe Lit(LitInt(1))
    rename(Lit(LitDouble(1))) shouldBe Lit(LitDouble(1))
    rename(Lit(LitBool(true))) shouldBe Lit(LitBool(true))
  }

  "x -> x' -> x" in {
    val term = Lambda("x", Lambda("x", Id("x")))
    val Lambda(var1, Lambda(var2, Id(id2))) = rename(term)
    Set(var1, var2) should have size (2)
    var2 shouldBe id2
  }

  "x -> x -> x' -> x" in {
    val term = Lambda("x", Lambda("x", Lambda("x'", Id("x"))))
    val Lambda(var1, Lambda(var2, Lambda(var3, Id(id2)))) = rename(term)
    Set(var1, var2, var3) should have size (3)
    var2 shouldBe id2
  }

  "leave unbound identifiers unaltered" in {
    rename(Id("x")) shouldBe Id("x")
  }

  "rename lambda bindings" in {
    val renamed = rename(Lambda("x", Lambda("x", Id("x"))))
    val Lambda(var1, Lambda(var2, Id(idLambda2))) = renamed
    Set(var1, var2) should have size (2)
    var2 shouldBe idLambda2
  }

  "rename let bindings" in {
    val renamed = rename(Let("x", Id("x"), Let("x", Id("x"), Id("x"))))
    val Let(x1, Id(id0), Let(x2, Id(id1), Id(id2))) = renamed
    Set(x1, x2) should have size (2)
    id0 shouldBe "x"
    id1 shouldBe x1
    id2 shouldBe x2
  }

  "perform renames adding 's" in {
    val renamed = rename(Lambda("x", Lambda("x", Lambda("x'", Id("x'")))))
    renamed shouldBe Lambda("x", Lambda("x'", Lambda("x''", Id("x''"))))
  }

  def rename(term: Term): Term = (new CaptureAvoidingRenamer).rename(term)
}
