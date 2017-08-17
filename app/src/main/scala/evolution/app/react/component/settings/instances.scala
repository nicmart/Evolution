package evolution.app.react.component.settings

import evolution.app.react.component.presentational.{DoubleInputComponent, NumericInputComponent}
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, _}
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, labelled}

object instances {
  import SettingsComponent._
  implicit val doubleSettings: SettingsComponent[Double] =
    SettingsComponent.instance[Double](props => DoubleInputComponent(props.settings, props.callback))

  implicit val intSettings: SettingsComponent[Int] =
    SettingsComponent.instance[Int](props => NumericInputComponent(props.settings, props.callback))

  implicit def seqSettings[T](implicit settings: SettingsComponent[T]): SettingsComponent[Seq[T]] =
    new SeqComponent[T](settings)

  implicit def optSettings[T](implicit settings: SettingsComponent[T]): SettingsComponent[Option[T]] =
    new OptionComponent[T](settings)

  implicit val hnilSettings: SettingsComponent[HNil] =
    SettingsComponent.instance[HNil](props => <.div())

  implicit def hlistSettings[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    hSettings: Lazy[SettingsComponent[H]],
    tSettings: SettingsComponent[T]
  ): SettingsComponent[FieldType[K, H] :: T] = {

    val fieldName = witness.value.name
    def hCallback(props: Props[FieldType[K, H] :: T])(h: H): Callback = {
      val hh: FieldType[K, H] = labelled.field[K](h)
      props.callback(hh :: props.settings.tail)
    }
    def tCallback(props: Props[FieldType[K, H] :: T])(t: T): Callback = {
      props.callback(props.settings.head :: t)
    }

    SettingsComponent.instance { props =>
      <.div(
        <.label(^.className := "label", fieldName),
        hSettings.value.component(Props(props.settings.head, hCallback(props))),
        tSettings.component(Props(props.settings.tail, tCallback(props)))
      )
    }
  }

  implicit def genericProductSettings[A, H <: HList](
    implicit
    generic: LabelledGeneric.Aux[A, H],
    hSettings: Lazy[SettingsComponent[H]]
  ): SettingsComponent[A] = {
    def callback(props: Props[A])(h: H): Callback = {
      props.callback(generic.from(h))
    }
    SettingsComponent.instance { props =>
      hSettings.value.component(Props(generic.to(props.settings), callback(props)))
    }
  }

  class SeqComponent[T](component: SettingsComponent[T]) extends SettingsComponent[Seq[T]] {
    import SettingsComponent.Props

    def component(props: Props[Seq[T]]): VdomElement = {
      val children: Seq[VdomElement] = props.settings.zipWithIndex.map { case (t, index) =>
        component.component(Props(t, onChangeElement(props, index)))
      }
      <.div(children.toTagMod)
    }

    private def onChangeElement(props: Props[Seq[T]], index: Int)(t: T): Callback =
      for {
        newSettings <- CallbackTo { props.settings.updated(index, t) }
        _ <- props.callback(newSettings)
      } yield ()
  }

  class OptionComponent[T](component: SettingsComponent[T]) extends SettingsComponent[Option[T]] {
    import SettingsComponent.Props

    def component(props: Props[Option[T]]): VdomElement = {
      props.settings.fold[VdomElement](<.span())(t => component.component(Props(t, onChange(props))))
    }

    private def onChange(props: Props[Option[T]])(t: T): Callback =
      props.callback(Some(t))
  }
}
