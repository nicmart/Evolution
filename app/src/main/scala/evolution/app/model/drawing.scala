package evolution.app.model

import evolution.app.react.component.EvolutionContextComponent
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.VdomElement
import paint.evolution.Evolution
import paint.evolution.generator.EvolutionGenerator

sealed trait Drawing[T] {
  type Context
  val name: String
  val generator: EvolutionGenerator.Aux[T, Context]
  val contextComponent: EvolutionContextComponent[Context]
  val context: Context

  def evolution: Evolution[T] = generator.evolution(context)

  def contextElement(callback: Drawing[T] => Callback): VdomElement = {
    contextComponent.component(EvolutionContextComponent.Props(
      context,
      ctx => callback(withContext(ctx))
    ))
  }

  def withContext(ctx: Context): Drawing.Aux[T, Context] =
    Drawing(name, generator, contextComponent, ctx)
}

object Drawing {
  type Aux[T, C] = Drawing[T] {type Context = C}

  def apply[T, C](
    _name: String,
    _generator: EvolutionGenerator.Aux[T, C],
    _component: EvolutionContextComponent[C],
    _context: C
  ): Aux[T, C] = new Drawing[T] {
    type Context = C
    val name = _name
    val generator = _generator
    val context = _context
    val contextComponent = _component
  }
}

case class DrawingList[T](drawings: List[Drawing[T]]) {
  def drawing(name: String): Option[Drawing[T]] =
    drawings.dropWhile(_.name != name).headOption
}

case class DrawingListWithSelection[T](
  list: DrawingList[T],
  current: Drawing[T]
) {
  def select(drawingName: String): DrawingListWithSelection[T] = {
    val newCurrent = list.drawing(drawingName).getOrElse(current)
    copy(current = newCurrent)
  }
}