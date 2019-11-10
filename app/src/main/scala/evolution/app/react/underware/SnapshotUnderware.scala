package evolution.app.react.underware

import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.Callback

object SnapshotUnderware {
  def simpleSnapshot[S](initial: S)(f: S => Callback): StateSnapshot[S] =
    StateSnapshot(initial) {
      case (Some(s), _) => f(s)
      case (None, clb)  => clb
    }
}
