package evolution.compiler.tree

import evolution.compiler.phases.typing.config.{ Constant2, Constant3 }
import scala.collection.immutable.Nil
import evolution.compiler.phases.typing.config.Constant1
import evolution.compiler.tree.TreeF._

object SpecialSyntax {
  def zip(bindings: List[(String, Tree)], body: Tree): Tree = bindings match {
    case h1 :: h2 :: tl =>
      variadicZipWith(buildLambda(bindings.map(_._1), body), h1._2, h2._2, tl.map(_._2))
    case h1 :: Nil => AppN(Const(Constant2.Map), h1._2, buildLambda(List(h1._1), body))
    case Nil       => body
  }

  def withFirst(binding: (String, Tree), body: Tree): Tree =
    TreeF.AppN(TreeF.Const(Constant2.WithFirst), binding._2, buildLambda(List(binding._1), body))

  def product(bindings: List[(String, Tree)], body: Tree): Tree = bindings match {
    case Nil       => body
    case h1 :: Nil => AppN(Const(Constant2.Map), h1._2, buildLambda(List(h1._1), body))
    case h1 :: tail =>
      AppN(Const(Constant2.FlatMap), h1._2, buildLambda(List(h1._1), product(tail, body)))
  }

  def functionBinding(name: String, args: List[String], value: Tree, body: Tree): Tree =
    Let(name, buildLambda(args, value), body).embed

  def uniformChoice(args: List[Tree]): Tree =
    AppN(Const(Constant1.UniformChoice), Lst(args).embed)

  private def buildLambda(vars: List[String], body: Tree): Tree =
    vars match {
      case Nil          => body
      case head :: tail => Lambda(head, buildLambda(tail, body)).embed
    }

  private def variadicZipWith(f: Tree, arg1: Tree, arg2: Tree, rest: List[Tree]): Tree =
    rest match {
      case Nil => AppN(Const(Constant3.ZipWith), arg1, arg2, f)
      case nonEmptyRest =>
        AppN(
          Const(Constant3.ZipWith),
          variadicZipWith(f, arg1, arg2, nonEmptyRest.dropRight(1)),
          nonEmptyRest.last,
          Lambda("f", Lambda("x", AppN(Identifier("f").embed, Identifier("x").embed)).embed).embed
        )
    }
}
