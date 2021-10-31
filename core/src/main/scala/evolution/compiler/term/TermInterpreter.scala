package evolution.compiler.term

import evolution.compiler.impl.evaluation.MaterializeNumeric
import evolution.compiler.phases.typer.config.ConstConfig
import evolution.compiler.term.RegisterBasedInterpreter.constants
import evolution.compiler.term.Term.Literal._
import evolution.compiler.term.Term._
import evolution.compiler.types.TypeClassInstance.NumericInst

trait TermInterpreter:
  def interpret(term: Term): Any

final class RegisterBasedInterpreter extends TermInterpreter:
  def interpret(term: Term): Any = interpretRec(constants)(term)

  def interpretRec(register: Map[String, Any])(term: Term): Any = term match
    case Lit(LitInt(n))      => (num: NumericInst[Any]) => MaterializeNumeric(num.num)(n)
    case Lit(LitBool(b))     => b
    case Lit(LitDouble(d))   => d
    case Lit(LitList(terms)) => terms.map(interpretRec(register))

    case Let(name, expr, body) =>
      val registerWithExpr = register.updated(name, interpretRec(register)(expr))
      interpretRec(registerWithExpr)(body)

    case Lambda(name, body) =>
      (x: Any) => interpretRec(register.updated(name, x))(body)

    case Apply(f, x) =>
      interpretRec(register)(f).asInstanceOf[Any => Any](interpretRec(register)(x))

    case Id(name) => register(name)

    case Inst(inst) => inst

    case Value(value) => value

object RegisterBasedInterpreter:
  def fresh: RegisterBasedInterpreter = RegisterBasedInterpreter()

  val constants: Map[String, Any] = ConstConfig.constants.map(c => c.name -> c.value).toMap
