package evolution.compiler.tree

import evolution.compiler.phases.typer.config.{Constant0, Constant1, Constant2, Constant3}

import scala.collection.immutable.Nil
import evolution.compiler.tree.TreeF._

object SpecialSyntax {
  def zip(bindings: List[(String, Tree)], body: Tree): Tree = bindings match {
    case h1 :: h2 :: tl =>
      variadicZipWith(buildLambda(bindings.map(_._1), body), h1._2, h2._2, tl.map(_._2))
    case h1 :: Nil => App.of(Identifier.const(Constant2.Map).embed, h1._2, buildLambda(List(h1._1), body)).embed
    case Nil       => body
  }

  def withFirst(binding: (String, Tree), body: Tree): Tree =
    TreeF.App
      .of(TreeF.Identifier.const(Constant2.WithFirst).embed, binding._2, buildLambda(List(binding._1), body))
      .embed

  def product(bindings: List[(String, Tree)], body: Tree): Tree = bindings match {
    case Nil       => body
    case h1 :: Nil => App.of(Identifier.const(Constant2.Map).embed, h1._2, buildLambda(List(h1._1), body)).embed
    case h1 :: tail =>
      App.of(Identifier.const(Constant2.FlatMap).embed, h1._2, buildLambda(List(h1._1), product(tail, body))).embed
  }

  def functionBinding(name: String, args: List[String], value: Tree, body: Tree): Tree =
    Let(name, buildLambda(args, value), body).embed

  def uniformChoice(args: List[Tree]): Tree =
    App.of(Identifier.const(Constant1.UniformChoice).embed, Lst(args).embed).embed

  def cons(asts: List[Tree]): Tree = asts match {
    case Nil          => TreeF.Identifier(Constant0.Empty.entryName).embed
    case head :: tail => App.of(Identifier.const(Constant2.Cons).embed, head, cons(tail)).embed
  }

  private def buildLambda(vars: List[String], body: Tree): Tree =
    vars match {
      case Nil          => body
      case head :: tail => Lambda(head, buildLambda(tail, body)).embed
    }

  private def variadicZipWith(f: Tree, arg1: Tree, arg2: Tree, rest: List[Tree]): Tree =
    rest match {
      case Nil => App.of(Identifier.const(Constant3.ZipWith).embed, arg1, arg2, f).embed
      case nonEmptyRest =>
        App
          .of(
            Identifier.const(Constant3.ZipWith).embed,
            variadicZipWith(f, arg1, arg2, nonEmptyRest.dropRight(1)),
            nonEmptyRest.last,
            Lambda("f", Lambda("x", App.of(Identifier("f").embed, Identifier("x").embed).embed).embed).embed
          )
          .embed
    }
}
