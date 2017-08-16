package evolution.app.react.component

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.VdomElement
import evolution.app.portfolio.EvolutionGeneratorPortfolio.brownian
import evolution.app.react.component.presentational.{DoubleInputComponent, NumericInputComponent}
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.html_<^._
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, labelled}
import shapeless.labelled.FieldType

trait EvolutionContextComponent[Context] {
  def component(props: EvolutionContextComponent.Props[Context]): VdomElement
}

object EvolutionContextComponent {
  case class Props[Context](
    context: Context,
    callback: Context => Callback
  )

  def instance[Context](render: Props[Context] => VdomElement): EvolutionContextComponent[Context] =
    new EvolutionContextComponent[Context] {
      override def component(props: Props[Context]): VdomElement =
        render(props)
    }

  def apply[Context](implicit context: EvolutionContextComponent[Context]): EvolutionContextComponent[Context]
    = context
}

object instances {
  import evolution.app.react.component.EvolutionContextComponent.Props
  implicit val doubleContext: EvolutionContextComponent[Double] =
    EvolutionContextComponent.instance[Double](props => DoubleInputComponent(props.context, props.callback))

  implicit val intContext: EvolutionContextComponent[Int] =
    EvolutionContextComponent.instance[Int](props => NumericInputComponent(props.context, props.callback))

  implicit def seqContext[T](implicit context: EvolutionContextComponent[T]): EvolutionContextComponent[Seq[T]] =
    new SeqComponent[T](context)

  implicit def optContext[T](implicit context: EvolutionContextComponent[T]): EvolutionContextComponent[Option[T]] =
    new OptionComponent[T](context)

  implicit val hnilContext: EvolutionContextComponent[HNil] =
    EvolutionContextComponent.instance[HNil](props => <.div())

  implicit def hlistContext[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    hContext: Lazy[EvolutionContextComponent[H]],
    tContext: EvolutionContextComponent[T]
  ): EvolutionContextComponent[FieldType[K, H] :: T] = {

    val fieldName = witness.value.name
    def hCallback(props: Props[FieldType[K, H] :: T])(h: H): Callback = {
      val hh: FieldType[K, H] = labelled.field[K](h)
      props.callback(hh :: props.context.tail)
    }
    def tCallback(props: Props[FieldType[K, H] :: T])(t: T): Callback = {
      props.callback(props.context.head :: t)
    }

    EvolutionContextComponent.instance { props =>
      <.div(
        <.label(^.className := "label", fieldName),
        hContext.value.component(Props(props.context.head, hCallback(props))),
        tContext.component(Props(props.context.tail, tCallback(props)))
      )
    }
  }

  implicit def genericProductContext[A, H <: HList](
    implicit
    generic: LabelledGeneric.Aux[A, H],
    hContext: Lazy[EvolutionContextComponent[H]]
  ): EvolutionContextComponent[A] = {
    def callback(props: Props[A])(h: H): Callback = {
      props.callback(generic.from(h))
    }
    EvolutionContextComponent.instance { props =>
      hContext.value.component(Props(generic.to(props.context), callback(props)))
    }
  }

  class SeqComponent[T](component: EvolutionContextComponent[T]) extends EvolutionContextComponent[Seq[T]] {
    import EvolutionContextComponent.Props

    def component(props: Props[Seq[T]]): VdomElement = {
      val children: Seq[VdomElement] = props.context.zipWithIndex.map { case (t, index) =>
        component.component(Props(t, onChangeElement(props, index)))
      }
      <.div(children.toTagMod)
    }

    private def onChangeElement(props: Props[Seq[T]], index: Int)(t: T): Callback =
      for {
        newContext <- CallbackTo { props.context.updated(index, t) }
        _ <- props.callback(newContext)
      } yield ()
  }

  class OptionComponent[T](component: EvolutionContextComponent[T]) extends EvolutionContextComponent[Option[T]] {

    import EvolutionContextComponent.Props

    def component(props: Props[Option[T]]): VdomElement = {
      props.context.fold[VdomElement](<.span())(t => component.component(Props(t, onChange(props))))
    }

    private def onChange(props: Props[Option[T]])(t: T): Callback =
      props.callback(Some(t))
  }

  object BrownianComponent extends EvolutionContextComponent[brownian.Context] {
    type Context = brownian.Context
    type Props = EvolutionContextComponent.Props[Context]

    class Backend(bs: BackendScope[Props, Unit]) {
      def render(props: Props): VdomElement = {
        <.div(
          DoubleInputComponent(props.context.radius, onRadiusChange(props)),
          DoubleInputComponent(props.context.start.x, onXChange(props)),
          DoubleInputComponent(props.context.start.y, onYChange(props))
        )
      }

      private def onRadiusChange(props: Props)(radius: Double): Callback = {
        props.callback(props.context.copy(radius = radius))
      }

      private def onXChange(props: Props)(x: Double): Callback = {
        val currentStart = props.context.start
        props.callback(props.context.copy(start = currentStart.copy(x = x)))
      }

      private def onYChange(props: Props)(y: Double): Callback = {
        val currentStart = props.context.start
        props.callback(props.context.copy(start = currentStart.copy(y = y)))
      }
    }

    def component(props: Props): VdomElement =
      ScalaComponent.builder[Props]("Component")
          .renderBackend[Backend]
          .build
          .apply(props)
  }

}
