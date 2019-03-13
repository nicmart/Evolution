package evolution.language
import cats.Id
import cats.implicits._
import cats.mtl.implicits._
import cats.kernel.{ Eq, Order }
import org.scalacheck.Gen

class CompilerModuleSpec extends LanguageSpec[Id] {
  import Expr._, Desugarer._, TypeClasses._

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
        unsafeCompile(AST.Let(variable.name, n1, n2)) shouldBe Let(
          variable.name,
          unsafeCompile(n1),
          unsafeCompile(n2)
        )
      }

      "lambdas" in forAll(genTypedVar, genTypedNumber) { (variable, n) =>
        unsafeCompile(AST.Lambda(variable.name, n)) shouldBe Lambda(variable.name, unsafeCompile(n))
      }

      "ands" in forAll(genBool, genBool) { (a, b) =>
        unsafeCompile(AST.App2(AST.PrimitiveConst(Constant2.And), a, b)) shouldBe And(
          unsafeCompile(a),
          unsafeCompile(b))
      }

      "ors" in forAll(genBool, genBool) { (a, b) =>
        unsafeCompile(AST.App2(AST.PrimitiveConst(Constant2.Or), a, b)) shouldBe Or(unsafeCompile(a), unsafeCompile(b))
      }

      "nots" in forAll(genBool) { a =>
        unsafeCompile(AST.App(AST.PrimitiveConst(Constant1.Not), a)) shouldBe Not(unsafeCompile(a))
      }

      "boolean literals" in forAll { b: Boolean =>
        unsafeCompile(AST.Bool(b)) shouldBe Bool(b)
      }

      "binary minus" in forAll { (a: Double, b: Double) =>
        val ast =
          AST.App2(
            AST.PrimitiveConst(Constant2.Minus),
            AST.Number(a.toString, Qualified(Type.Dbl)),
            AST.Number(b.toString, Qualified(Type.Dbl)))
        unsafeCompile(ast) shouldBe Add(Dbl(a), Inverse(Dbl(b)))
      }

      "whiles" in forAll(genBool, genNumber) { (b, n) =>
        val predicate = AST.Lambda("x", b)
        val evolution = AST.App2(AST.PrimitiveConst(Constant2.Cons), n, AST.PrimitiveConst(Constant0.Empty))
        val expected = takeWhile[Double](unsafeCompile(evolution), unsafeCompile(predicate))
        unsafeCompile(AST.App2(AST.PrimitiveConst(Constant2.While), evolution, predicate)) shouldBe expected
      }

      "untils" in forAll(genBool, genNumber) { (b, n) =>
        val predicate = AST.Lambda("x", b)
        val evolution = AST.App2(AST.PrimitiveConst(Constant2.Cons), n, AST.PrimitiveConst(Constant0.Empty))
        val expected = takeUntil[Double](unsafeCompile(evolution), unsafeCompile(predicate))
        unsafeCompile(AST.App2(AST.PrimitiveConst(Constant2.Until), evolution, predicate)) shouldBe expected
      }

      "equality operators" in forAll(equalityOperators[Double], genTypedNumber, genTypedNumber) {
        case ((ast, f), a, b) =>
          unsafeCompile(AST.App2(ast, a, b)) shouldBe f(unsafeCompile(a), unsafeCompile(b))
      }

      "relation operators" in forAll(relationOperators[Double], genTypedNumber, genTypedNumber) {
        case ((ast, f), a, b) =>
          unsafeCompile(AST.App2(ast, a, b)) shouldBe f(unsafeCompile(a), unsafeCompile(b))
      }

      "liftings" - {
        "of numbers" in {
          unsafeCompile(AST.App(AST.PrimitiveConst(Constant1.Lift), AST.Number("1"))) shouldBe constant(Dbl(1))
        }
      }
    }
  }

  def equalityOperators[T: Eq]: Gen[(AST, (Expr[T], Expr[T]) => Expr[Boolean])] =
    Gen.oneOf(
      AST.PrimitiveConst(Constant2.Eq) -> Equals.apply[T] _,
      AST.PrimitiveConst(Constant2.Neq) -> Neq.apply[T] _
    )

  def relationOperators[T: Order]: Gen[(AST, (Expr[T], Expr[T]) => Expr[Boolean])] =
    Gen.oneOf(
      AST.PrimitiveConst(Constant2.GreaterThan) -> GreaterThan.apply[T] _,
      AST.PrimitiveConst(Constant2.GreaterThanOrEqual) -> GreaterThanOrEqual.apply[T] _,
      AST.PrimitiveConst(Constant2.LessThan) -> LessThan.apply[T] _,
      AST.PrimitiveConst(Constant2.LessThanOrEqual) -> LessThanOrEqual.apply[T] _
    )

  private def unsafeCompile[T](expr: AST, ctx: VarContext = VarContext.empty): Expr[T] =
    Compiler.compile[Either[String, ?]](expr).run(ctx).fold(s => throw new Exception(s), _.asInstanceOf[Expr[T]])
}
