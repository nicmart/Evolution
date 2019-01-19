package evolution.primitive

object ast {

  sealed trait Expr
  object Expr {
    case class Var(name: String) extends Expr
    case class FuncCall(funcName: String, args: List[Expr]) extends Expr
    case class Lambda(varName: String, expr: Expr) extends Expr
    case class Dbl(d: Double) extends Expr
    case class Int(n: Int) extends Expr
  }
}
