import shapeless.the

object Test1 {
  sealed trait Type[T]
  case object Integer extends Type[Int]
  case class Func[T1, T2](`type1`: Type[T1], `type2`: Type[T2]) extends Type[T1 => T2]

  sealed abstract class Expr[T](val `type`: Type[T])
  case class Number(n: Int) extends Expr[Int](Integer)
  case class Add(a: Expr[Int], b: Expr[Int]) extends Expr[Int](Integer)
  case class Variable[T: Type](name: String) extends Expr[T](the[Type[T]])
  case class Lambda[T1: Type, T2](varname: String, expr: Expr[T2])
      extends Expr[T1 => T2](Func(the[Type[T1]], expr.`type`))
  case class App[T1, T2: Type](f: Expr[T1 => T2], t1: Expr[T1]) extends Expr[T2](the[Type[T2]])

  val vars: Map[String, Any] = ???

  trait Compiler[T] {
    def compile(a: Expr[T]): T
  }

  object Compiler {
    def instance[T](f: Expr[T] => T): Compiler[T] = f(_)
    def apply[T](implicit compiler: Compiler[T]): Compiler[T] = compiler
  }

  implicit class Ops[T](expr: Expr[T]) {
    def compile: T = compiler(expr.`type`).compile(expr)
  }

  def compiler[T](`type`: Type[T]): Compiler[T] = `type` match {
    case Integer =>
      Compiler.instance {
        case Variable(varname) => vars.get(varname).asInstanceOf[Int]
        case Number(n)         => n
        case Add(n1, n2)       => n1.compile + n2.compile
        case App(f, t1)        => f.compile(t1.compile)
      }
    case _: Func[t1, t2] =>
      Compiler.instance {
        case Variable(name) => vars.get(name).asInstanceOf[t1 => t2]
        case Lambda(varname, t2) =>
          t1 => t2.compile // Just for the sake of simplicity, we should push t1 (compiled) on the stack
        case App(f, t1) => f.compile(t1.compile)
      }
  }
}

object Test2 {
  sealed trait Lst[T]
  sealed trait Type[T]

  case object TNum extends Type[Int]
  case class TFunc[T1, T2](`type1`: Type[T1], `type2`: Type[T2]) extends Type[T1 => T2]
  case class TLst[T](`type`: Type[T]) extends Type[Lst[T]]

  implicit val numType: Type[Int] = TNum
  implicit def lstType[T](implicit tpe: Type[T]): Type[Lst[T]] = TLst(tpe)
  implicit def funcType[T1, T2](implicit type1: Type[T1], type2: Type[T2]): Type[T1 => T2] = TFunc(type1, type2)

  sealed abstract class Expr[T](implicit val `type`: Type[T])

  case class Number(n: Int) extends Expr[Int]
  case class Cons[T: Type](h: Expr[T], t: Expr[Lst[T]]) extends Expr[Lst[T]]
  case class Empty[T: Type]() extends Expr[Lst[T]]
  case class Map[T1: Type, T2: Type](as: Expr[Lst[T1]], f: Expr[T1 => T2]) extends Expr[Lst[T2]]
  case class Constant[T1: Type, T2: Type](c: Expr[T2]) extends Expr[T1 => T2]
  case object PlusOne extends Expr[Int => Int]
  case class Head[T: Type]() extends Expr[Lst[T] => T]

  val n: Expr[Int] = Number(10)
  val lst: Expr[Lst[Int]] = Cons(n, Empty())
  val lstLst: Expr[Lst[Lst[Int]]] = Cons(lst, Empty())

  sealed trait Compiler[A, B] {
    def compile(expr: Expr[A]): B
  }

  object Compiler {
    def apply[T1, T2](implicit c: Compiler[T1, T2]): Compiler[T1, T2] = c
    def instance[A, B](f: Expr[A] => B): Compiler[A, B] =
      new Compiler[A, B] {
        def compile(expr: Expr[A]): B = f(expr)
      }
  }

  implicit val intCompiler: Compiler[Int, Int] = new Compiler[Int, Int] {
    override def compile(expr: Expr[Int]): Int = expr match {
      case Number(int) => int
    }
  }

  implicit def headCompiler[T1, T2](implicit c: Compiler[Lst[T1], List[T2]]): Compiler[Lst[T1] => T1, List[T2] => T2] =
    Compiler.instance {
      case Head() => list => list.head
    }

  implicit def funcCompiler[A1, A2, B1, B2](
    implicit c1: Compiler[A1, A2],
    c2: Compiler[B1, B2]
  ): Compiler[A1 => B1, A2 => B2] =
    Compiler.instance {
      case Constant(c) => _ => c2.compile(c)
      case _: Head[t]  => lst => ???
    }

  implicit def lstCompiler[T1, T2](implicit innerCompiler: Compiler[T1, T2]): Compiler[Lst[T1], List[T2]] =
    new Compiler[Lst[T1], List[T2]] {
      override def compile(expr: Expr[Lst[T1]]): List[T2] = expr match {
        case Empty()          => List.empty
        case Cons(head, tail) => innerCompiler.compile(head) :: compile(tail)
        case Map(as, f)       => ???
      }
    }

  val compilableLstLst: Compiler[Lst[Lst[Int]], List[List[Int]]] = implicitly
}
