package evolution.compiler.systemf

import evolution.compiler.systemf.TermCompiler.Ctx
import cats.implicits._
import evolution.compiler.impl.evaluation.MaterializeAddition
import evolution.compiler.types.TypeClassInstance
import evolution.compiler.types.TypeClassInstance._

class TermCompiler {
  def compile[T](term: Term[T]): Either[String, Ctx => Any] =
    term match {
      case Term.Var(name, tpe) => Right(_.binding(name))

      case Term.Integer(n) => Right(_ => n)

      case Term.Let(varName, term, in) =>
        (compile(term), compile(in)).mapN {
          case (term, in) => ctx: Ctx => in(ctx.withBinding(varName, term(ctx)))
        }

      case Term.Lambda(varName, varType, term) =>
        compile(term).map { body => ctx: Ctx => x: Any =>
          body(ctx.withBinding(varName, x))
        }

      case Term.App(f, x) =>
        (compile(f), compile(x)).mapN {
          case (f, x) =>
            val fFunc = f.asInstanceOf[Ctx => Any => Any]
            ctx => fFunc(ctx)(x)
        }

      case Term.TLambda(_, term) => compile(term) // Erase types

      case Term.TApp(f, _) => compile(f) // Erase types

      case Term.PLambda(_, term) =>
        compile(term).map { term => (ctx: Ctx) => (inst: TypeClassInstance) =>
          term(ctx.withInstance(inst))
        }

      case Term.PApp(f, instance) =>
        compile(f).map { f =>
          val fFunc = f.asInstanceOf[Ctx => TypeClassInstance => Any]
          (ctx: Ctx) => fFunc(ctx)(instance)
        }

      case Term.PApp(Term.PLambda(_, term), AdditiveInst(add)) =>
        compile(term).map { term => (ctx: Ctx) =>
          // TODO we should have an already-curried materialization
          ctx.withBinding("add", (a: Any) => (b: Any) => MaterializeAddition(add)(a, b))
        }
    }
}

object TermCompiler {
  class Ctx(bindings: Map[String, Any], instances: List[TypeClassInstance]) {
    def binding(name: String): Any = bindings(name)
    def withBinding(name: String, value: Any): Ctx = new Ctx(bindings = bindings.updated(name, value), instances)
    def withInstance(instance: TypeClassInstance): Ctx = new Ctx(bindings, instance :: instances)
  }

  object Ctx {
    val empty = new Ctx(Map.empty, List.empty)
  }
}
