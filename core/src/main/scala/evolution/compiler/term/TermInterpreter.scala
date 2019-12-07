package evolution.compiler.term

import scala.collection.mutable
import Term._
import Literal._
import evolution.compiler.expression.typeclass.{Additive, Numeric}
import evolution.compiler.impl.evaluation.{MaterializeAddition, MaterializeNumeric}
import evolution.compiler.term.Term.PArg.{PInst, PVar}
import evolution.compiler.tree.TreeF.IntLiteral
import evolution.compiler.types.TypeClassInstance
import evolution.compiler.types.TypeClassInstance.{AdditiveInst, NumericInst}

// F will be String for JsMaterialization, Register => Any for EvalMaterialization
class TermInterpreter {
  def interpret(term: Term): Any = RegisterBasedInterpreter.fresh.interpret(term)
}

class RegisterBasedInterpreter private (
    register: mutable.Map[String, Any],
    instancesRegister: mutable.Map[String, Any]
) {
  def interpret(term: Term): Any = term match {
    case Lit(LitInt(n))        => n
    case Lit(LitBool(b))       => b
    case Lit(LitDouble(d))     => d
    case Lit(LitList(terms))   => ???
    case Id(name)              => register(name)
    case Let(name, expr, body) => ???
    case Lambda(name, body) =>
      (x: Any) => {
        bind(name, x) // TODO this is not local
        interpret(body)
      }
    case App(f, x) =>
      interpret(f).asInstanceOf[Any => Any](interpret(x))

    case PLambda(pName, body) => ???

    case PApp(Lit(LitInt(n)), PInst(NumericInst(num))) =>
      MaterializeNumeric(num)(n)

    case PApp(Lit(LitInt(n)), PVar(name)) =>
      MaterializeNumeric(instancesRegister(name).asInstanceOf[Numeric[Any]])(n)

    case PApp(Id("add"), PInst(AdditiveInst(add))) =>
      val f = MaterializeAddition(add) // TODO curry Materialization for PERFOMANCE!
      (x: Any) => (y: Any) => f(x, y)

    case PApp(Id("add"), PVar(name)) =>
      val f = MaterializeAddition(instancesRegister(name).asInstanceOf[Additive[Any, Any, Any]]) // TODO curry Materialization for PERFOMANCE!
      (x: Any) => (y: Any) => f(x, y)
  }

  def bind(name: String, value: Any): Unit = register.update(name, value)
  def bindInstance(name: String, instance: Any): Unit = instancesRegister.update(name, instance)
}

object RegisterBasedInterpreter {
  def fresh: RegisterBasedInterpreter =
    new RegisterBasedInterpreter(mutable.Map.empty, mutable.Map.empty)
}
