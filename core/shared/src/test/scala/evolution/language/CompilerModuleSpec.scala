package evolution.language
import cats.Id
import cats.implicits._
import cats.mtl.implicits._
import cats.kernel.{ Eq, Order }
import org.scalacheck.Gen

class CompilerModuleSpec extends LanguageSpec[Id] {
  import Expr._, TypeClasses._

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
        unsafeCompile(AST.AppN(AST.PrimitiveConst(Constant2.And), a, b)) shouldBe And(
          unsafeCompile(a),
          unsafeCompile(b)
        )
      }

      "ors" in forAll(genBool, genBool) { (a, b) =>
        unsafeCompile(AST.AppN(AST.PrimitiveConst(Constant2.Or), a, b)) shouldBe Or(unsafeCompile(a), unsafeCompile(b))
      }

      "nots" in forAll(genBool) { a =>
        unsafeCompile(AST.App(AST.PrimitiveConst(Constant1.Not), a)) shouldBe Not(unsafeCompile(a))
      }

      "boolean literals" in forAll { b: Boolean =>
        unsafeCompile(AST.Bool(b)) shouldBe Bool(b)
      }

      "binary minus" in forAll { (a: Double, b: Double) =>
        val ast =
          AST.AppN(
            AST.PrimitiveConst(Constant2.Minus),
            AST.Number(a.toString, Qualified(Type.Dbl)),
            AST.Number(b.toString, Qualified(Type.Dbl))
          )
        unsafeCompile(ast) shouldBe Add(Dbl(a), Inverse(Dbl(b)))
      }

      "whiles" in forAll(genBool, genNumber) { (b, n) =>
        val predicate = AST.Lambda("x", b)
        val evolution = AST.AppN(AST.PrimitiveConst(Constant2.Cons), n, AST.PrimitiveConst(Constant0.Empty))
        val expected = TakeWhile[Double](unsafeCompile(evolution), unsafeCompile(predicate))
        unsafeCompile(AST.AppN(AST.PrimitiveConst(Constant2.While), evolution, predicate)) shouldBe expected
      }

      "equality operators" in forAll(equalityOperators[Double], genTypedNumber, genTypedNumber) {
        case ((ast, f), a, b) =>
          unsafeCompile(AST.AppN(ast, a, b)) shouldBe f(unsafeCompile(a), unsafeCompile(b))
      }

      "relation operators" in forAll(relationOperators[Double], genTypedNumber, genTypedNumber) {
        case ((ast, f), a, b) =>
          unsafeCompile(AST.AppN(ast, a, b)) shouldBe f(unsafeCompile(a), unsafeCompile(b))
      }

      "constant" - {
        "of numbers" in {
          unsafeCompile(AST.App(AST.PrimitiveConst(Constant1.Constant), AST.Number("1"))) shouldBe Expr.Constant(Dbl(1))
        }
      }

      "zipWith" - {
        "of vars" in {
          unsafeCompile(
            AST.AppN(
              AST.PrimitiveConst(Constant3.ZipWith),
              AST.Identifier("x"),
              AST.Identifier("y"),
              AST.Identifier("z")
            ),
            new VarContext(List("x", "y", "z"))
          ) shouldBe ZipWith(Var[Any]("x"), Var[Any]("y"), Var[Any => Any => Any]("z"))
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
