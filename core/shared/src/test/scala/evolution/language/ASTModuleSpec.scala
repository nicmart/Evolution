package evolution.language
import evolution.compiler.ast.AST

class ASTModuleSpec extends LanguageSpec {
  "An AST" - {
    "should recursively transform all the nodes" in {
      def changeNumbers(ast: AST): AST = ast match {
        case AST.DoubleLiteral(n, t) => AST.DoubleLiteral(2 * n, t)
        case tree                    => tree
      }

      val expression = AST.AppN(AST.Const(Constant2.Add), AST.DoubleLiteral(1), AST.DoubleLiteral(2))
      val transformed = AST.transformRecursively(expression, changeNumbers)

      transformed shouldBe AST.AppN(AST.Const(Constant2.Add), AST.DoubleLiteral(2), AST.DoubleLiteral(4))
    }
  }
}
