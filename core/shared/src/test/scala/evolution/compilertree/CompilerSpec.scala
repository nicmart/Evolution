package evolution.compilertree
import cats.implicits._
import cats.kernel.{ Eq, Order }
import cats.data.NonEmptyList
import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalacheck.Arbitrary.arbitrary
import evolution.data.Expr
import evolution.data.Expr._
import evolution.compilertree.phases.compiling.model.VarContext
import evolution.compilertree.phases.compiling.Compile
import evolution.compilertree.phases.typing.config.{ Constant0, Constant1, Constant2, Constant3 }
import evolution.compilertree.types.TypeClasses._
import evolution.compilertree.types.Type
import evolution.compilertree.ast.TreeF
import evolution.compilertree.ast.TreeF.CoTree
import evolution.compilertree.ast.TreeF.TypedTree

class CompilerSpec extends LanguageSpec {
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
  "The compiler" - {
    "should successfully compile" - {
      "number literals" in forAll(arbitrary[Double]) { n =>
        unsafeCompile(TreeF.DoubleLiteral(n).annotate(Qualified(Type.Dbl))) shouldBe Dbl(n.toDouble)
      }

      "variable usages" in forAll(genTypedVar) {
        case (name, v) =>
          println(name)
          println(v)
          unsafeCompile(v, VarContext.empty.push(name)) shouldBe Var(name)
      }

      "variable usages in non-empty contexts" in forAll(genTypedVar) {
        case (name, v) =>
          whenever(name != "x") {
            unsafeCompile(v, VarContext.empty.push(name).push("x")) shouldBe Var(name)
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
          TreeF.App(TreeF.TypedPrimitiveConst(Constant2.And, unknownType), NonEmptyList.of(a, b)).withNoType
        ) shouldBe And(
          unsafeCompile(a),
          unsafeCompile(b)
        )
      }

      "ors" in forAll(genTypedBool, genTypedBool) { (a, b) =>
        unsafeCompile(
          TreeF.App(TreeF.TypedPrimitiveConst(Constant2.Or, unknownType), NonEmptyList.of(a, b)).withNoType
        ) shouldBe Or(
          unsafeCompile(a),
          unsafeCompile(b)
        )
      }

      "nots" in forAll(genTypedBool) { a =>
        unsafeCompile(
          TreeF.App(TreeF.TypedPrimitiveConst(Constant1.Not, unknownType), NonEmptyList.of(a)).withNoType
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
              TreeF.TypedPrimitiveConst(Constant2.Minus, unknownType),
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
            TreeF.TypedPrimitiveConst(Constant2.Cons, unknownType),
            NonEmptyList.of(
              n,
              TreeF.TypedPrimitiveConst(Constant0.Empty, unknownType)
            )
          )
          .withNoType
        val expected = TakeWhile[Double](unsafeCompile(evolution), unsafeCompile(predicate))

        unsafeCompile(
          TreeF
            .App(TreeF.TypedPrimitiveConst(Constant2.While, unknownType), NonEmptyList.of(evolution, predicate))
            .withNoType
        ) shouldBe expected
      }

      "equality operators" in forAll(equalityOperators[Int], genTypedNumber, genTypedNumber) {
        case ((ast, f), a, b) =>
          unsafeCompile(TreeF.App(ast, NonEmptyList.of(a, b)).withNoType) shouldBe f(
            unsafeCompile(a),
            unsafeCompile(b)
          )
      }

      "relation operators" in forAll(relationOperators[Int], genTypedNumber, genTypedNumber) {
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
                TreeF.TypedPrimitiveConst(Constant1.Constant, unknownType),
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
            CoTree(
              unknownType,
              TreeF.App(
                TreeF.TypedPrimitiveConst(Constant3.ZipWith, unknownType),
                NonEmptyList.of(
                  CoTree(unknownType, TreeF.Identifier("x")),
                  CoTree(unknownType, TreeF.Identifier("y")),
                  CoTree(unknownType, TreeF.Identifier("z"))
                )
              )
            ),
            new VarContext(List("x", "y", "z"))
          ) shouldBe ZipWith(Var("x"), Var("y"), Var[Any => Any => Any]("z"))
        }
      }

      "uniformChoice" in {
        val compiled = unsafeCompile(
          TreeF
            .App(
              TreeF.TypedPrimitiveConst(Constant1.UniformChoice, unknownType),
              NonEmptyList.of(
                TreeF
                  .Lst(
                    List(TreeF.Identifier("x"), TreeF.Identifier("y"), TreeF.Identifier("z")).map(_.withNoType)
                  )
                  .withNoType
              )
            )
            .withNoType,
          new VarContext(List("x", "y", "z"))
        )
        compiled shouldBe UniformChoice(Lst(List(Var("x"), Var("y"), Var("z"))))
      }
    }
  }

  def equalityOperators[T: Eq]: Gen[(TypedTree, (Expr[T], Expr[T]) => Expr[Boolean])] =
    Gen.oneOf(
      TreeF.TypedPrimitiveConst(Constant2.Eq, unknownType) -> (Equals.apply[T](_, _, Eq[T])),
      TreeF.TypedPrimitiveConst(Constant2.Neq, unknownType) -> (Neq.apply[T](_, _, Eq[T]))
    )

  def relationOperators[T: Order]: Gen[(TypedTree, (Expr[T], Expr[T]) => Expr[Boolean])] =
    Gen.oneOf(
      TreeF.TypedPrimitiveConst(Constant2.GreaterThan, unknownType) -> (GreaterThan[T](_, _, Order[T])),
      TreeF.TypedPrimitiveConst(Constant2.GreaterThanOrEqual, unknownType) -> (GreaterThanOrEqual[T](_, _, Order[T])),
      TreeF.TypedPrimitiveConst(Constant2.LessThan, unknownType) -> (LessThan[T](_, _, Order[T])),
      TreeF.TypedPrimitiveConst(Constant2.LessThanOrEqual, unknownType) -> (LessThanOrEqual[T](_, _, Order[T]))
    )

  lazy val unknownType: Qualified[Type] = Qualified(Type.Var(""))
  lazy val intType: Qualified[Type] = Qualified(Type.Integer)

  private def unsafeCompile[T](expr: TypedTree, ctx: VarContext = VarContext.empty): Expr[T] =
    Compile.compile(expr, ctx).unsafeEvaluate.asInstanceOf[Expr[T]]

  implicit class Ops(tree: TreeF[CoTree[Qualified[Type]]]) {
    def withNoType = tree.annotate(unknownType)
    def withType(tpe: Type) = tree.annotate(Qualified(tpe))
  }
}
