package evolution.app.model

import evolution.app.model.context.DrawingContext
import evolution.compiler.term.{Definition, Module, Term}
import evolution.compiler.types.Type
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.Qualified

object DrawingContextModule {
  def apply(ctx: DrawingContext): Module =
    Module(
      List(
        Definition("top", Some(Term.Value(ctx.top)), Qualified(Scheme(Type.Double))),
        Definition("bottom", Some(Term.Value(ctx.bottom)), Qualified(Scheme(Type.Double))),
        Definition("left", Some(Term.Value(ctx.left)), Qualified(Scheme(Type.Double))),
        Definition("right", Some(Term.Value(ctx.right)), Qualified(Scheme(Type.Double)))
      )
    )
}
