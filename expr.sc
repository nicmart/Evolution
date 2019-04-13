import scala.language.higherKinds

sealed trait Expr[T]

case class Number(n: Int) extends Expr[Int]

sealed trait Lst[T]

case class Cons[T](h: Expr[T], t: Expr[Lst[T]]) extends Expr[Lst[T]]
case class Empty[T]() extends Expr[Lst[T]]

val n: Expr[Int] = Number(10)
val lst: Expr[Lst[Int]] = Cons(n, Empty())
val lstLst: Expr[Lst[Lst[Int]]] = Cons(lst, Empty())


//def compile(n: Expr[Int]): Int
// def compile(lst: Expr[Lst[Int]]): List[Int]
// def compile(expr: Expr[G[Lst]]: G[List]

def compile[G[_[_]]](expr: Expr[G[Lst]]): G[List] = ???








