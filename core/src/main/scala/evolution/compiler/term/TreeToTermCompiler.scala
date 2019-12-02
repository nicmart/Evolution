package evolution.compiler.term

import evolution.compiler.tree.{TreeF, TypedTree}
import cats.implicits._
import Term._
import Term.Literal._

class TreeToTermCompiler {
  def compile(tree: TypedTree): Either[String, Term] = tree.tree match {
    case TreeF.Id(name, primitive)    => ???
    case TreeF.Lambda(varName, expr)  => ???
    case TreeF.App(f, args)           => ???
    case TreeF.Let(varName, expr, in) => ???

    case TreeF.DoubleLiteral(n) => Right(Lit(LitDouble(n)))
    case TreeF.IntLiteral(n)    => Right(Lit(LitInt(n)))
    case TreeF.Bool(b)          => Right(Lit(LitBool(b)))
    case TreeF.Lst(ts)          => ts.traverse(compile).map(ts => Lit(LitList(ts)))
  }
}
