package evolution.app.react.component

import evolution.app.react.component.config.ConfigComponent.Props
import japgolly.scalajs.react.CtorType
import japgolly.scalajs.react.component.Scala.Component

package object config {
  type ConfigComponent[Config] = Component[Props[Config], Unit, Unit, CtorType.Props]
}
