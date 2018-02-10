package evolution.thoery.lang.higherorder

import evolution.theory.lang.higherorder.{Evaluate, Serialize}
import evolution.theory.lang.higherorder.parser._
import org.scalatest.{Matchers, WordSpec}

class LangSpec extends WordSpec with Matchers {
  "An higher-order language with let-bindings" should {
    "parse and evaluate expressions correctly" in {

      evaluate[Int]("1") shouldBe 1
      evaluate[Int]("add(1,1)") shouldBe 2
      evaluate[Int]("let(x,1,$x)") shouldBe 1
      evaluate[Int]("let(a,1,add($a,$a))") shouldBe 2
      evaluate[Int]("let(foo,7,add($foo,let(bar,5,add($bar,add($bar,add(2,3))))))") shouldBe 22
      evaluate[Int]("let(x,1,let(y,2,add($x,$y)))") shouldBe 3
      reserialize[Int]("let(x,1,let(z,2,add($x,$z)))") shouldBe "let(x, 1, let(z, 2, add($x, $z)))"
      evaluate[Int]("let(x,1,let(y,2,add($x,$y)))") shouldBe 3
      reserialize[Int]("let(x,1,let(y,2,add($x,$y)))") shouldBe "let(x, 1, let(y, 2, add($x, $y)))"

      // Scoping
      evaluate[Int]("let(x,2,add($x,let(x,3,add($x,$x))))") shouldBe 8
      reserialize[Int]("let(x,2,add($x,let(x,3,add($x,$x))))") shouldBe "let(x, 2, add($x, let(x, 3, add($x, $x))))"
      evaluate[Int]("let(x,5,let(y,10,add($x,add($y,$x))))") shouldBe 20

      evaluate[Boolean]("true") shouldBe true
      evaluate[Boolean]("let(x,true,let(y,false,$y))") shouldBe false

      // Ifs
      evaluate[Int]("if(true,1,0)") shouldBe 1
      evaluate[Int]("let(x,false,if($x,1,0))") shouldBe 0
    }

    "chooser test" in {
      import ChoosePOC._
      choose("asdasd", 12) shouldBe "asdasd"
      choose("asdasd", "x") shouldBe "x"

      def test[T, U](t: T, u: U): T = choose(t, u)

      test("asdasd", 12) shouldBe "asdasd"
      // This fails, because the implicit resolution happens once in the definition of  test
      //test("asdasd", "x") shouldBe "x"
    }
  }

  def evaluate[T: Type](serializedExpression: String): T =
    initialParser.get[T].parse(serializedExpression).get.value.run(Evaluate)(())
  def reserialize[T: Type](serializedExpression: String): String =
    initialParser.get[T].parse(serializedExpression).get.value.run(Serialize)(Nil)
}
