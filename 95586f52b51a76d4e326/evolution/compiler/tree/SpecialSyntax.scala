package evolution.compiler.tree

import scala.collection.immutable.Nil
import evolution.compiler.tree.Tree._

object SpecialSyntax {
  def zip(bindings: List[(String, Tree)], body: Tree): Tree = bindings match {
    case h1 :: h2 :: tl =>
      variadicZipWith(buildLambda(bindings.map(_._1), body), h1._2, h2._2, tl.map(_._2))
    case h1 :: Nil => App.of(Id("map"), h1._2, buildLambda(List(h1._1), body))
    case Nil       => body
  }

  def withFirst(binding: (String, Tree), body: Tree): Tree =
    App
      .of(Id("withfirst"), binding._2, buildLambda(List(binding._1), body))

  def product(bindings: List[(String, Tree)], body: Tree): Tree = bindings match {
    case Nil       => body
    case h1 :: Nil => App.of(Id("map"), h1._2, buildLambda(List(h1._1), body))
    case h1 :: tail =>
      App.of(Id("flatmap"), h1._2, buildLambda(List(h1._1), product(tail, body)))
  }

  def functionBinding(name: String, args: List[String], value: Tree, body: Tree): Tree =
    Let(name, buildLambda(args, value), body)

  def uniformChoice(args: List[Tree]): Tree =
    App.of(Id("uniformchoice"), Lst(args))

  def cons(asts: List[Tree]): Tree = asts match {
    case Nil          => Id("empty")
    case head :: tail => App.of(Id("cons"), head, cons(tail))
  }

  private def buildLambda(vars: List[String], body: Tree): Tree =
    vars match {
      case Nil          => body
      case head :: tail => Lambda(head, buildLambda(tail, body))
    }

  private def variadicZipWith(f: Tree, arg1: Tree, arg2: Tree, rest: List[Tree]): Tree =
    rest match {
      case Nil => App.of(Id("zipwith"), arg1, arg2, f)
      case nonEmptyRest =>
        App
          .of(
            Id("zipwith"),
            variadicZipWith(f, arg1, arg2, nonEmptyRest.dropRight(1)),
            nonEmptyRest.last,
            Lambda("f", Lambda("x", App.of(Id("f"), Id("x"))))
          )
    }
}
