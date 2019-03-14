package evolution.app.react.component.config

import evolution.app.react.component.presentational.styled.FormField
import evolution.app.react.component.presentational.{ DoubleInputComponent, IntInputComponent, TextArea }
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import shapeless.labelled.FieldType
import shapeless.{ ::, HList, HNil, LabelledGeneric, Lazy, Witness, labelled }

object instances {
  import ConfigComponent._

  val empty: VdomElement = <.div()

  implicit val unitConfig: ConfigComponent[Unit] =
    instance("unit config")((_, children) => <.div(children))

  implicit val doubleConfig: ConfigComponent[Double] =
    instance[Double]("double config") { (props, _) =>
      DoubleInputComponent.component(props)
    }

  implicit val intConfig: ConfigComponent[Int] =
    instance[Int]("int config") { (props, _) =>
      IntInputComponent.component(props)
    }

  implicit val textConfig: ConfigComponent[String] =
    instance[String]("string config") { (props, _) =>
      TextArea.component(props)
    }

  implicit def seqConfig[T](implicit configComponent: ConfigComponent[T]): ConfigComponent[Seq[T]] =
    SeqConfigComponent(configComponent)

  implicit def optConfig[T](implicit configComponent: ConfigComponent[T]): ConfigComponent[Option[T]] =
    OptionalConfigComponent(configComponent)

  implicit val hnilConfig: ConfigComponent[HNil] =
    instance[HNil]("hnil config")((_, children) => <.div(children))

  implicit def hlistConfig[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    hConfig: Lazy[ConfigComponent[H]],
    tConfigs: ConfigComponent[T]
  ): ConfigComponent[FieldType[K, H] :: T] = {

    val fieldName = witness.value.name

    ConfigComponent.instance("hlist config") { (props, children) =>
      val hSnapshot = props.zoomState[H](_.head)(h => hlist => labelled.field[K](h) :: hlist.tail)
      val tSnapshot = props.zoomState[T](_.tail)(t => hlist => hlist.head :: t)
      val headElement = FormField.component(FormField.Props(fieldName)) {
        hConfig.value(hSnapshot)()
      }
      tConfigs(tSnapshot)(children, headElement)
    }
  }

  implicit def genericProductConfig[A, H <: HList](
    implicit
    generic: LabelledGeneric.Aux[A, H],
    hConfig: Lazy[ConfigComponent[H]]
  ): ConfigComponent[A] = {
    ConfigComponent.instance("generic product config") { (props, children) =>
      val aSnapshot = props.zoomState[H](a => generic.to(a))(h => _ => generic.from(h))
      hConfig.value(aSnapshot)(children)
    }
  }
}
