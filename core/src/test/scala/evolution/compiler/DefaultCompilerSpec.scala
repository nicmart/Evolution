package evolution.compiler
import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalacheck.Arbitrary.arbitrary
import evolution.compiler.expression.Expr
import evolution.compiler.expression.Expr._
import evolution.compiler.phases.compiling.DefaultCompiler
import evolution.compiler.phases.typing.config.{ Constant0, Constant1, Constant2, Constant3 }
import evolution.compiler.types.TypeClasses._
import evolution.compiler.types.Type
import evolution.compiler.types.TypeT
import evolution.compiler.tree.TreeF
import evolution.compiler.tree._
import evolution.compiler.expression.typeclass._
import evolution.compiler.module.Module

class DefaultCompilerSpec extends LanguageSpec {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
  "The compiler" - {
    "should successfully compile" - {
      "number literals" in forAll(arbitrary[Double]) { n =>
        unsafeCompile(TreeF.DoubleLiteral(n).annotate(Qualified(TypeT.Double))) shouldBe Dbl(n.toDouble)
      }

      "variable usages" in forAll(genTypedVar) {
        case (name, v) =>
          println(name)
          println(v)
          unsafeCompile(v) shouldBe Var(name)
      }

      "variable usages in non-empty contexts" in forAll(genTypedVar) {
        case (name, v) =>
          whenever(name != "x") {
            unsafeCompile(v) shouldBe Var(name)
          }
      }

      "let bindings" in forAll(genTypedVar, genTypedNumber, genTypedNumber) {
        case ((id, _), n1, n2) =>
          unsafeCompile(TreeF.Let(id, n1, n2).withNoType) shouldBe Let(
            id,
            unsafeCompile(n1),
            unsafeCompile(n2)
          )
      }

      "lambdas" in forAll(genTypedVar, genTypedNumber) {
        case ((id, _), n) =>
          unsafeCompile(TreeF.Lambda(id, n).withNoType) shouldBe Lambda(id, unsafeCompile(n))
      }

      "ands" in forAll(genTypedBool, genTypedBool) { (a, b) =>
        unsafeCompile(
          TreeF.App(TreeF.Identifier.primitiveConst(Constant2.And).withNoType, NonEmptyList.of(a, b)).withNoType
        ) shouldBe And(
          unsafeCompile(a),
          unsafeCompile(b)
        )
      }

      "ors" in forAll(genTypedBool, genTypedBool) { (a, b) =>
        unsafeCompile(
          TreeF.App(TreeF.Identifier.primitiveConst(Constant2.Or).withNoType, NonEmptyList.of(a, b)).withNoType
        ) shouldBe Or(
          unsafeCompile(a),
          unsafeCompile(b)
        )
      }

      "nots" in forAll(genTypedBool) { a =>
        unsafeCompile(
          TreeF.App(TreeF.Identifier.primitiveConst(Constant1.Not).withNoType, NonEmptyList.of(a)).withNoType
        ) shouldBe Not(
          unsafeCompile(a)
        )
      }

      "boolean literals" in forAll { b: Boolean =>
        unsafeCompile(TreeF.Bool(b).withNoType) shouldBe Bool(b)
      }

      "binary minus" in forAll { (a: Int, b: Int) =>
        val ast =
          TreeF
            .App(
              TreeF.Identifier.primitiveConst(Constant2.Minus).withNoType,
              NonEmptyList.of(
                TreeF.IntLiteral(a).annotate(intType),
                TreeF.IntLiteral(b).annotate(intType)
              )
            )
            .withNoType
        unsafeCompile(ast) should matchPattern {
          case Expr.Minus(Expr.Integer(x), Expr.Integer(y), _, _) if x == a && y == b =>
        }
      }

      "whiles" in forAll(genTypedBool, genTypedNumber) { (b, n) =>
        val predicate = TreeF.Lambda("x", b).withNoType
        val evolution = TreeF
          .App(
            TreeF.Identifier.primitiveConst(Constant2.Cons).withNoType,
            NonEmptyList.of(
              n,
              TreeF.Identifier.primitiveConst(Constant0.Empty).withNoType
            )
          )
          .withNoType
        val expected = TakeWhile[Double](unsafeCompile(evolution), unsafeCompile(predicate))

        unsafeCompile(
          TreeF
            .App(TreeF.Identifier.primitiveConst(Constant2.While).withNoType, NonEmptyList.of(evolution, predicate))
            .withNoType
        ) shouldBe expected
      }

      "equality operators" in forAll(equalityOperators(Equable.Int), genTypedNumber, genTypedNumber) {
        case ((ast, f), a, b) =>
          unsafeCompile(TreeF.App(ast, NonEmptyList.of(a, b)).withNoType) shouldBe f(
            unsafeCompile(a),
            unsafeCompile(b)
          )
      }

      "relation operators" in forAll(relationOperators(Comparable.Int), genTypedNumber, genTypedNumber) {
        case ((ast, f), a, b) =>
          unsafeCompile(TreeF.App(ast, NonEmptyList.of(a, b)).withNoType) shouldBe f(
            unsafeCompile(a),
            unsafeCompile(b)
          )
      }

      "constant" - {
        "of numbers" in {
          unsafeCompile(
            TreeF
              .App(
                TreeF.Identifier.primitiveConst(Constant1.Constant).withNoType,
                NonEmptyList.of(TreeF.DoubleLiteral(1).withNoType)
              )
              .withNoType
          ) shouldBe Expr.Constant(
            Dbl(1)
          )
        }
      }

      "zipWith" - {
        "of vars" in {
          unsafeCompile(
            TreeF.App
              .of(
                TreeF.Identifier.primitiveConst(Constant3.ZipWith).withNoType,
                TreeF.Identifier("x").withNoType,
                TreeF.Identifier("y").withNoType,
                TreeF.Identifier("z").withNoType
              )
              .withNoType
          ) shouldBe ZipWith(Var("x"), Var("y"), Var[Any => Any => Any]("z"))
        }
      }

      "uniformChoice" in {
        val compiled = unsafeCompile(
          TreeF.App
            .of(
              TreeF.Identifier.primitiveConst(Constant1.UniformChoice).withNoType,
              TreeF
                .Lst(
                  List(TreeF.Identifier("x"), TreeF.Identifier("y"), TreeF.Identifier("z")).map(_.withNoType)
                )
                .withNoType
            )
            .withNoType
        )
        compiled shouldBe UniformChoice(Lst(List(Var("x"), Var("y"), Var("z"))))
      }
    }
  }

  def equalityOperators[T](eq: Equable[T]): Gen[(TypedTree, (Expr[T], Expr[T]) => Expr[Boolean])] =
    Gen.oneOf(
      TreeF.Identifier.primitiveConst(Constant2.Eq).withNoType -> (Equals.apply[T](_, _, eq)),
      TreeF.Identifier.primitiveConst(Constant2.Neq).withNoType -> (Neq.apply[T](_, _, eq))
    )

  def relationOperators[T](cmp: Comparable[T]): Gen[(TypedTree, (Expr[T], Expr[T]) => Expr[Boolean])] =
    Gen.oneOf(
      TreeF.Identifier.primitiveConst(Constant2.GreaterThan).withNoType -> (GreaterThan[T](_, _, cmp)),
      TreeF.Identifier.primitiveConst(Constant2.GreaterThanOrEqual).withNoType -> (GreaterThanOrEqual[T](_, _, cmp)),
      TreeF.Identifier.primitiveConst(Constant2.LessThan).withNoType -> (LessThan[T](_, _, cmp)),
      TreeF.Identifier.primitiveConst(Constant2.LessThanOrEqual).withNoType -> (LessThanOrEqual[T](_, _, cmp))
    )

  lazy val unknownType: Qualified[Type] = Qualified(TypeT.Var(""))
  lazy val intType: Qualified[Type] = Qualified(TypeT.Integer)

  private def unsafeCompile[T](expr: TypedTree): Expr[T] =
    DefaultCompiler.compile(expr, Module.empty).unsafeEvaluate.asInstanceOf[Expr[T]]

  implicit class Ops(tree: TreeF[TypedTree]) {
    def withNoType = tree.annotate(unknownType)
    def withType(tpe: Type) = tree.annotate(Qualified(tpe))
  }
}
