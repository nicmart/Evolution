package evolution.app.react.component.settings

import evolution.app.portfolio.EvolutionGeneratorPortfolio.brownian
import evolution.app.react.component.presentational.{DoubleInputComponent, NumericInputComponent}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, _}
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, labelled}

trait SettingsComponent[Settings] {
  import SettingsComponent._
  def component(props: Props[Settings]): VdomElement
}

object SettingsComponent {
  case class Props[Settings](
    settings: Settings,
    callback: Settings => Callback
  )

  def instance[Settings](render: Props[Settings] => VdomElement): SettingsComponent[Settings] =
    new SettingsComponent[Settings] {
      override def component(props: Props[Settings]): VdomElement =
        render(props)
    }

  /**
    * Summoner method
    */
  def apply[Settings](implicit component: SettingsComponent[Settings]): SettingsComponent[Settings]
    = component
}


