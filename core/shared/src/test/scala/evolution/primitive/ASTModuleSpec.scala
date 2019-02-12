package evolution.primitive
import cats.Id
import org.scalatest.{ FreeSpec, Matchers }

class ASTModuleSpec extends CompilerSpecModule[Id] {
  import ast._
  "An AST" - {
    "should recursively transform all the nodes" in {
      def changeNumbers(ast: AST): AST = ast match {
        case AST.Number(n, t) => AST.Number(s"$n $n", t)
        case tree             => tree
      }

      val expression = AST.App2(AST.Const(PredefinedConstant.Add), AST.Number("1"), AST.Number("2"))
      val transformed = AST.transformRecursively(expression, changeNumbers)

      transformed shouldBe AST.App2(AST.Const(PredefinedConstant.Add), AST.Number("1 1"), AST.Number("2 2"))
    }
  }
}
