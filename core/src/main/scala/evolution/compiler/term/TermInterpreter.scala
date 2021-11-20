package evolution.compiler.term

import evolution.compiler.phases.typer.config.NativeSymbolsConfig
import evolution.compiler.term.RegisterBasedInterpreter.nativeSymbols
import evolution.compiler.term.Term.Literal.*
import evolution.compiler.term.Term.*

trait TermInterpreter:
  def interpret(term: Term): Any

class RegisterBasedInterpreter extends TermInterpreter:
  def interpret(term: Term): Any = interpretRec(nativeSymbols)(term)

  def interpretRec(register: Map[String, Any])(term: Term): Any = term match
    case Lit(LitInt(n))      => (num: Function1[Int, Any]) => num(n)
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

    case Id(name)     => register(name)
    case Inst(inst)   => inst.value
    case Value(value) => value

object RegisterBasedInterpreter:
  def fresh: RegisterBasedInterpreter = RegisterBasedInterpreter()

  val nativeSymbols: Map[String, Any] = NativeSymbolsConfig.symbols.map(c => c.symbol -> c.value).toMap
