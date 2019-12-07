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
    case Lit(LitInt(n))        => (num: NumericInst[Any]) => MaterializeNumeric(num.num)(n)
    case Lit(LitBool(b))       => b
    case Lit(LitDouble(d))     => d
    case Lit(LitList(terms))   => ???
    case Let(name, expr, body) => ???
    case Lambda(name, body) =>
      (x: Any) => {
        bind(name, x) // TODO this is not local
        interpret(body)
      }
    case App(f, x) =>
      interpret(f).asInstanceOf[Any => Any](interpret(x))

    case PLambda(pName, body) => ???

    case Id(name) => register(name)

    case PApp(term, PInst(inst)) =>
      interpret(term).asInstanceOf[Any => Any](inst)

    case PApp(term, PVar(pName)) =>
      interpret(term).asInstanceOf[Any => Any](instancesRegister(pName))
  }

  def bind(name: String, value: Any): Unit = register.update(name, value)
  def bindInstance(name: String, instance: Any): Unit = instancesRegister.update(name, instance)
}

object RegisterBasedInterpreter {
  def fresh: RegisterBasedInterpreter =
    new RegisterBasedInterpreter(mutable.Map.from(constants), mutable.Map.empty)

  val constants: Map[String, Any] = Map(
    "add" -> { (p: AdditiveInst[Any, Any, Any]) => (x: Any) => (y: Any) =>
      MaterializeAddition(p.add)(x, y)
    }
  )
}
