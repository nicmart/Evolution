package evolution.app.react.component.config

import evolution.app.model.definition.{CompositeDefinitionConfig, DrawingListWithSelection}
import evolution.app.react.component.DrawingList
import evolution.app.react.component.presentational.styled.FormField
import evolution.app.react.component.presentational.{DoubleInputComponent, IntInputComponent}
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, _}
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, labelled}
import io.circe.syntax._
import io.circe.generic.auto._
import japgolly.scalajs.react.extra.StateSnapshot

object instances {
  import ConfigComponent._

  val empty: VdomElement = <.div()

  implicit val unitConfig: ConfigComponent[Unit] =
    instance("unit config")((props, children) => <.div(children))

  implicit val doubleConfig: ConfigComponent[Double] =
    instance[Double]("double config") {
      (props, children) => DoubleInputComponent.component(props)
    }

  implicit val intConfig: ConfigComponent[Int] =
    instance[Int]("int config") {
      (props, children) => IntInputComponent.component(props)
    }

  implicit def seqConfig[T](implicit configComponent: ConfigComponent[T]): ConfigComponent[Seq[T]] =
    SeqComponent(configComponent)

  implicit def optConfig[T](implicit configComponent: ConfigComponent[T]): ConfigComponent[Option[T]] =
    OptionalConfig(configComponent)

  implicit val hnilConfig: ConfigComponent[HNil] =
    instance[HNil]("hnil config")((props, children) => <.div(children))

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
      val aSnapshot = props.zoomState[H](a => generic.to(a))(h => a => generic.from(h))
      hConfig.value(aSnapshot)(children)
    }
  }

  object SeqComponent {
    def apply[T](innerComponent: ConfigComponent[T]): ConfigComponent[Seq[T]] =
      instance[Seq[T]]("sequence config") { (props, children) =>
        val components = for {
            i <- props.value.indices
            snapshot = tSnapshot(props)(i)
            component = innerComponent(snapshot)()
          } yield component
        <.div(components.toTagMod)
      }

    private def tSnapshot[T](props: StateSnapshot[Seq[T]])(index: Int): StateSnapshot[T] =
      props.zoomState(_.apply(index))(t => ts => ts.updated(index, t))
  }

  object OptionalConfig {
    def apply[T](component: ConfigComponent[T]): ConfigComponent[Option[T]] =
      instance("optional config") { (props, children) =>
        def tSnapshot(t: T): StateSnapshot[T] = props.zoomState(_ => t)(tt => opt => Some(tt))
        props.value.fold(empty)(t => component(tSnapshot(t))())
      }
  }

  object CompositeConfigComponent {

    def apply[T](drawingList: DrawingListWithSelection[T]): ConfigComponent[CompositeDefinitionConfig[T]] = {
      val drawingListComponent = DrawingList.component[T]

      instance("composite config component") { (props, children) =>
        val config = props.value
        val innerComponent: ConfigComponent[config.InnerConfig] =
          config.definition.configComponent
        val innerConfig: config.InnerConfig = config.config

        val dropdown = FormField.component(FormField.Props("Drawing")) {
          <.div(
            drawingListComponent(
              DrawingList.Props(
                drawingList.list,
                config.definition,
                newDefinition => props.setState(
                  CompositeDefinitionConfig[T, newDefinition.Config](
                    newDefinition.initialConfig,
                    newDefinition
                  )
                )
              )
            )
          )
        }

        val innerConfigComp = innerComponent(
          StateSnapshot[config.InnerConfig](innerConfig)
            (newInnerConfig => props.setState(CompositeDefinitionConfig(newInnerConfig, config.definition)))
        )()

        <.div(^.className := "inner-config", dropdown, innerConfigComp)
      }
    }
  }

}

