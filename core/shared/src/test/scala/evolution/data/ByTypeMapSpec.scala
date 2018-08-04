package evolution.data

import org.scalatest.{Matchers, WordSpec}
import ByTypeMap._
import shapeless.HNil
import shapeless.test.illTyped

class ByTypeMapSpec extends WordSpec with Matchers {
  "A TypeByMap" should {
    "retrieve values by type" in {
      val byTypeMap = HNil.add("string").add(123).add('a')
      byTypeMap.get[String] shouldBe "string"
      byTypeMap.get[Int] shouldBe 123
      byTypeMap.get[Char] shouldBe 'a'
    }

    "fail compilation if a value is not found" in {
      illTyped("""get[String](Empty)""")
      val byTypeMap = "string" :: 123 :: 'a' :: HNil
      illTyped("""get[Float](byTypeMap)""")
    }
  }
}
