package evolution.app.react.component.presentational

import scalajs.js
import scala.scalajs.js.annotation.JSName
import japgolly.scalajs.react._
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.annotation.JSImport

object CodeMirror {

  //@JSGlobal("Controlled")
  @JSImport("react-codemirror2", "UnControlled")
  @js.native
  object RawComponent extends js.Object

  @js.native
  trait Editor extends js.Object

  @js.native
  trait Data extends js.Object

  type OnChange = js.Function3[Editor, Data, String, Unit]

  @js.native
  trait Props extends js.Object {
    var value: String = js.native
    var onChange: OnChange = js.native
  }

  def props(value: String, onChange: (Editor, Data, String) => Callback): Props = {
    val p = (new js.Object).asInstanceOf[Props]
    p.value = value
    p.onChange = (editor: Editor, data: Data, value: String) => onChange(editor, data, value).runNow()
    p
  }

  val component = JsComponent[Props, Children.Varargs, Null](RawComponent)
}
