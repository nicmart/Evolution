object Test {
  sealed trait Type[T]
  object Type {
    object Number extends Type[Int]
    case class Chain[T](inner: Type[T]) extends Type[List[T]]
    case class Function[A, B](from: Type[A], to: Type[B]) extends Type[A => B]
  }

  sealed trait Expr[T]
  object Expr {
    case class Number(n: Int) extends Expr[Int]

    object Chain {
      case class Empty[T]() extends Expr[List[T]]
      case class Cons[T](head: T, tail: Expr[List[T]]) extends Expr[List[T]]
      case class Map[A, B](f: Expr[A => B], as: Expr[List[A]]) extends Expr[List[B]]
    }

    object Function {
      case class Constant[A, B](b: B) extends Expr[A => B]
      case class IntFunc(f: Int => Int) extends Expr[Int => Int]
      case class Head[T]() extends Expr[List[T] => T]
    }
  }

  import Expr._, Chain._, Function._
  val simpleList = Cons(Number(12), Cons(Number(13), Empty()))
  val mappedSimpleList = Map(IntFunc(_ + 1), simpleList)

  val nestedList = Cons(simpleList, Cons(simpleList, Empty()))
  val mappedNestedList = Map(Head(), nestedList)
}
