package evolution.compiler.term

import evolution.compiler.LanguageSpec
import Term.*
import evolution.compiler.term.Term.Literal.LitList

class TermRenamerTest extends LanguageSpec:
  "convert identifiers" in {
    convert("x", "y")(Id("x")) `shouldBe` Id("y")
  }

  "do not convert identifiers with different names" in {
    convert("x", "y")(Id("z")) `shouldBe` Id("z")
  }

  "convert let terms" in {
    val term = Let("z", Id("x"), Id("x"))
    convert("x", "y")(term) `shouldBe` Let("z", Id("y"), Id("y"))
  }

  "do not convert shadowed identifiers in let terms bodies" in {
    val term = Let("x", Id("x"), Id("x"))
    convert("x", "y")(term) `shouldBe` Let("x", Id("y"), Id("x"))
  }

  "convert lambda terms" in {
    val term = Lambda("z", Id("x"))
    convert("x", "y")(term) `shouldBe` Lambda("z", Id("y"))
  }

  "do not convert shadowed identifiers in lambda terms" in {
    val term = Lambda("x", Id("x"))
    convert("x", "y")(term) `shouldBe` term
  }

  "convert lists" in {
    val term = Lit(LitList(List(Id("x"), Id("y"))))
    convert("x", "y")(term) `shouldBe` Lit(LitList(List(Id("y"), Id("y"))))
  }

  lazy val convert: (String, String) => Term => Term = TermRenamer.rename
