package evolution.app.react.component.config

import evolution.app.react.component.presentational.{DoubleInputComponent, NumericInputComponent}
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, _}
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, labelled}

object instances {

  import ConfigComponent._

  implicit val doubleConfig: ConfigComponent[Double] =
    ConfigComponent.instance[Double](props => DoubleInputComponent(props.config, props.callback))

  implicit val intConfig: ConfigComponent[Int] =
    ConfigComponent.instance[Int](props => NumericInputComponent(props.config, props.callback))

  implicit def seqConfig[T](implicit configComponent: ConfigComponent[T]): ConfigComponent[Seq[T]] =
    new SeqComponent[T](configComponent)

  implicit def optConfig[T](implicit configComponent: ConfigComponent[T]): ConfigComponent[Option[T]] =
    new OptionComponent[T](configComponent)

  implicit val hnilConfig: ConfigComponent[HNil] =
    ConfigComponent.instance[HNil](props => <.div())

  implicit def hlistConfig[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    hConfig: Lazy[ConfigComponent[H]],
    tConfigs: ConfigComponent[T]
  ): ConfigComponent[FieldType[K, H] :: T] = {

    val fieldName = witness.value.name

    def hCallback(props: Props[FieldType[K, H] :: T])(h: H): Callback = {
      val hh: FieldType[K, H] = labelled.field[K](h)
      props.callback(hh :: props.config.tail)
    }

    def tCallback(props: Props[FieldType[K, H] :: T])(t: T): Callback = {
      props.callback(props.config.head :: t)
    }

    ConfigComponent.instance { props =>
      <.div(
        <.label(^.className := "label", fieldName),
        hConfig.value.component(Props(props.config.head, hCallback(props))),
        tConfigs.component(Props(props.config.tail, tCallback(props)))
      )
    }
  }

  implicit def genericProductConfig[A, H <: HList](
    implicit
    generic: LabelledGeneric.Aux[A, H],
    hConfig: Lazy[ConfigComponent[H]]
  ): ConfigComponent[A] = {
    def callback(props: Props[A])(h: H): Callback = {
      props.callback(generic.from(h))
    }

    ConfigComponent.instance { props =>
      hConfig.value.component(Props(generic.to(props.config), callback(props)))
    }
  }

  class SeqComponent[T](component: ConfigComponent[T]) extends ConfigComponent[Seq[T]] {

    import ConfigComponent.Props

    def component(props: Props[Seq[T]]): VdomElement = {
      val children: Seq[VdomElement] = props.config.zipWithIndex.map { case (t, index) =>
        component.component(Props(t, onChangeElement(props, index)))
      }
      <.div(children.toTagMod)
    }

    private def onChangeElement(props: Props[Seq[T]], index: Int)(t: T): Callback =
      for {
        newConfig <- CallbackTo {
          props.config.updated(index, t)
        }
        _ <- props.callback(newConfig)
      } yield ()
  }

  class OptionComponent[T](component: ConfigComponent[T]) extends ConfigComponent[Option[T]] {

    import ConfigComponent.Props

    def component(props: Props[Option[T]]): VdomElement = {
      props.config.fold[VdomElement](<.span())(t => component.component(Props(t, onChange(props))))
    }

    private def onChange(props: Props[Option[T]])(t: T): Callback =
      props.callback(Some(t))
  }

}
