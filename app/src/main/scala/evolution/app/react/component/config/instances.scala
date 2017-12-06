package evolution.app.react.component.config

import evolution.app.model.definition.{CompositeDefinitionConfig, DrawingListWithSelection}
import evolution.app.react.component.DrawingList
import evolution.app.react.component.presentational.styled.{FormField, HorizontalFormField}
import evolution.app.react.component.presentational.{DoubleInputComponent, IntInputComponent}
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, _}
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, labelled}
import io.circe.syntax._
import io.circe.generic.auto._

object instances {
//
  import ConfigComponent._
//
  implicit val unitConfig: ConfigComponent[Unit] =
    instance("unit config")(props => props.render(Nil))
//
  implicit val doubleConfig: ConfigComponent[Double] =
    instance[Double]("double config") {
      props => DoubleInputComponent(props.config, props.callback)
    }

  implicit val intConfig: ConfigComponent[Int] =
    instance[Int]("int config") {
      props => IntInputComponent(props.config, props.callback)
    }

  implicit def seqConfig[T](implicit configComponent: ConfigComponent[T]): ConfigComponent[Seq[T]] =
    SeqComponent(configComponent)

  implicit def optConfig[T](implicit configComponent: ConfigComponent[T]): ConfigComponent[Option[T]] =
    OptionalConfig(configComponent)
//
  implicit val hnilConfig: ConfigComponent[HNil] =
    instance[HNil]("hnil config")(props => props.render(Nil))

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

    ConfigComponent.instance("hlist config") { props =>
      val headElement = FormField.component(FormField.Props(
        fieldName,
        "",
        <.div(
          hConfig.value(Props(props.config.head, hCallback(props), props.render))
        )
      ))

      tConfigs(Props(props.config.tail, tCallback(props), prepend(headElement, props.render)))
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

    ConfigComponent.instance("generic product config") { props =>
      hConfig.value(Props(generic.to(props.config), callback(props), props.render))
    } }

  object SeqComponent {
    def apply[T](innerComponent: ConfigComponent[T]): ConfigComponent[Seq[T]] =
      instance[Seq[T]]("sequence config") { props =>
        val configsWithIndex = props.config.toList.zipWithIndex
        val render = configsWithIndex.foldLeft[List[VdomElement] => VdomElement](props.render) { (accRender, indexedConfig) =>
          prepend(
            innerComponent(
              Props(
                indexedConfig._1,
                onChangeElement(props, indexedConfig._2),
                props.render
              )
            ),
            accRender
          )
        }
        render(Nil)
      }

    private def onChangeElement[T](props: Props[Seq[T]], index: Int)(t: T): Callback =
      for {
        newConfig <- CallbackTo {
          props.config.updated(index, t)
        }
        _ <- props.callback(newConfig)
      } yield ()
  }
//
  object OptionalConfig {
    def apply[T](component: ConfigComponent[T]): ConfigComponent[Option[T]] =
      instance("optional config") { props =>
        props.config.fold[VdomElement](props.render(Nil)) { t =>
          component(Props(t, onChange(props), props.render))
        }
      }

    private def onChange[T](props: Props[Option[T]])(t: T): Callback =
      props.callback(Some(t))
  }

  object CompositeConfigComponent {

    def apply[T](drawingList: DrawingListWithSelection[T]): ConfigComponent[CompositeDefinitionConfig[T]] = {
      val drawingListComponent = DrawingList.component[T]

      instance("composite config component") { props =>
        val config = props.config
        val innerComponent: ConfigComponent[config.InnerConfig] =
          config.definition.configComponent
        val innerConfig: config.InnerConfig = config.config

        val dropdown = FormField.component(FormField.Props(
          "Drawing",
          "",
          <.div(
            drawingListComponent(
              DrawingList.Props(
                drawingList.list,
                props.config.definition,
                newDefinition => props.callback(
                  CompositeDefinitionConfig[T, newDefinition.Config](
                    newDefinition.initialConfig,
                    newDefinition
                  )
                )
              )
            )
          )
        ))

        val innerConfigComp = innerComponent(ConfigComponent.Props[config.InnerConfig](
          innerConfig,
          newInnerConfig => props.callback(CompositeDefinitionConfig(newInnerConfig, config.definition)),
          list => <.div(list.toTagMod)
        ))

        <.div(^.className := "inner-config", dropdown, innerConfigComp)
      }
    }
  }

}

