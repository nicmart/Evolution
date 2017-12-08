package evolution.app.react.component

import japgolly.scalajs.react.CtorType
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.extra.StateSnapshot

package object config {
  type ConfigComponent[Config] = Component[StateSnapshot[Config], Unit, Unit, CtorType.PropsAndChildren]
}
