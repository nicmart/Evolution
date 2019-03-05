package evolution.language
import cats.Id
import cats.implicits._

class CompilerModuleSpec extends LanguageSpec[Id] {
  import Expr._, Desugarer._

  "The compiler" - {
    "should successfully compile" - {
      "number literals" in forAll(genTypedNumber) { n =>
        unsafeCompile(n) shouldBe Dbl(n.n.toDouble)
      }

      "variable usages" in forAll(genTypedVar) { v =>
        unsafeCompile(v, VarContext.empty.push(v.name)) shouldBe Var[v.Out](v.name)
      }

      "variable usages in non-empty contexts" in forAll(genTypedVar) { v =>
        whenever(v.name != "x") {
          unsafeCompile(v, VarContext.empty.push(v.name).push("x")) shouldBe Var[v.Out](v.name)
        }
      }

      "let bindings" in forAll(genTypedVar, genTypedNumber, genTypedNumber) { (variable, n1, n2) =>
        val variable1 = AST.Var(variable.name, n1.tpe)
        unsafeCompile(AST.Let(variable1, n1, n2)) shouldBe Let(
          variable1.name,
          unsafeCompile(n1),
          unsafeCompile(n2)
        )
      }

      "lambdas" in forAll(genTypedVar, genTypedNumber) { (variable, n) =>
        unsafeCompile(AST.Lambda(variable, n)) shouldBe Lambda(variable.name, unsafeCompile(n))
      }

      "ands" in forAll(genBool, genBool) { (a, b) =>
        unsafeCompile(AST.App2(AST.Const(Constant.And), a, b)) shouldBe And(unsafeCompile(a), unsafeCompile(b))
      }

      "ors" in forAll(genBool, genBool) { (a, b) =>
        unsafeCompile(AST.App2(AST.Const(Constant.Or), a, b)) shouldBe Or(unsafeCompile(a), unsafeCompile(b))
      }

      "nots" in forAll(genBool) { a =>
        unsafeCompile(AST.App(AST.Const(Constant.Not), a)) shouldBe Not(unsafeCompile(a))
      }

      "boolean literals" in forAll { b: Boolean =>
        unsafeCompile(AST.Bool(b)) shouldBe Bool(b)
      }

      "binary minus" in forAll { (a: Double, b: Double) =>
        val ast =
          AST.App2(AST.Const(Constant.Minus), AST.Number(a.toString, Type.Dbl), AST.Number(b.toString, Type.Dbl))
        unsafeCompile(ast) shouldBe Add(Dbl(a), Inverse(Dbl(b)))
      }

      "whiles" in forAll(genBool, genNumber) { (b, n) =>
        val predicate = AST.Lambda(AST.Var("x"), b)
        val evolution = AST.App2(AST.Const(Constant.Cons), n, AST.Const(Constant.Empty))
        val expected = takeWhile[Double](unsafeCompile(evolution), unsafeCompile(predicate))
        unsafeCompile(AST.App2(AST.Const(Constant.While), evolution, predicate)) shouldBe expected
      }

      "untils" in forAll(genBool, genNumber) { (b, n) =>
        val predicate = AST.Lambda(AST.Var("x"), b)
        val evolution = AST.App2(AST.Const(Constant.Cons), n, AST.Const(Constant.Empty))
        val expected = takeUntil[Double](unsafeCompile(evolution), unsafeCompile(predicate))
        unsafeCompile(AST.App2(AST.Const(Constant.Until), evolution, predicate)) shouldBe expected
      }
    }
  }

  private def unsafeCompile[T](expr: AST, ctx: VarContext = VarContext.empty): Expr[T] =
    Compiler.compile[Either[String, ?]](expr).run(ctx).right.get.asInstanceOf[Expr[T]]
}
