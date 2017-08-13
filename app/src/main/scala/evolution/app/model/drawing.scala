package evolution.app.model

import paint.evolution.Evolution
import paint.evolution.generator.EvolutionGenerator

sealed trait Drawing[T] {
    type Context
    val name: String
    val generator: EvolutionGenerator.Aux[T, Context]
    val context: Context

    def evolution: Evolution[T] = generator.evolution(context)
    def withContext(ctx: Context): Drawing.Aux[T, Context] =
        Drawing(name, generator, ctx)
}

object Drawing {
    type Aux[T, C] = Drawing[T] { type Context = C }
    def apply[T, C](_name: String, _generator: EvolutionGenerator.Aux[T, C], _context: C): Aux[T, C] = new Drawing[T] {
        type Context = C
        val name = _name
        val generator = _generator
        val context = _context
    }
}

case class DrawingList[T](drawings: List[Drawing[T]]) {
    def drawing(name: String): Option[Drawing[T]] =
        drawings.dropWhile(_.name != name).headOption
}

case class DrawingContext[T](
    list: DrawingList[T],
    current: Drawing[T]
) {
    def select(drawingName: String): DrawingContext[T] = {
        val newCurrent = list.drawing(drawingName).getOrElse(current)
        copy(current = newCurrent)
    }
}