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
        Definition("top", Term.Value(ctx.top), Qualified(Scheme(Type.Double))),
        Definition("bottom", Term.Value(ctx.bottom), Qualified(Scheme(Type.Double))),
        Definition("left", Term.Value(ctx.left), Qualified(Scheme(Type.Double))),
        Definition("right", Term.Value(ctx.right), Qualified(Scheme(Type.Double)))
      )
    )
}
