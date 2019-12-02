package evolution.compiler.term

import evolution.compiler.tree.{AnnotatedTree, TreeF, TypedTree}
import cats.implicits._
import Term._
import Term.Literal._
import evolution.compiler.term.TreeToTermCompiler.Register
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}

class TreeToTermCompiler {
  def compile(tree: TypedTree): Either[String, Term] = ???

  def compileM(typedTree: TypedTree): Register => Either[String, Term] = {
    val AnnotatedTree(Qualified(predicates, tpe), tree) = typedTree

    register =>
      tree match {
        case TreeF.Id(name, primitive) =>
          predicates
            .filter(_.hasTypeVars)
            .traverse(register.get)
            .map(pApp(Id(name), _))
        case TreeF.Lambda(varName, expr)  => ???
        case TreeF.App(f, args)           => ???
        case TreeF.Let(varName, expr, in) => ???

        case TreeF.DoubleLiteral(n) => Right(Lit(LitDouble(n)))
        case TreeF.IntLiteral(n)    => Right(Lit(LitInt(n)))
        case TreeF.Bool(b)          => Right(Lit(LitBool(b)))
        case TreeF.Lst(ts)          => ts.traverse(compile).map(ts => Lit(LitList(ts)))
      }
  }

  private def pApp(term: Term, predicateNames: List[String]): Term =
    predicateNames.foldLeft(term) { case (term, name) => PApp() }
}

object TreeToTermCompiler {
  type PredName = String
  case class Register(map: Map[Predicate, PredName]) {
    def get(predicate: Predicate): Either[String, PredName] =
      map.get(predicate).toRight(s"Unable to find predicate $predicate in register")
  }

  object Register {
    val empty = Register(Map.empty)
  }
}
