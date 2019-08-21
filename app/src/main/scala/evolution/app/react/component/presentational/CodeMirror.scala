package evolution.app.react.component.presentational

import scalajs.js
import scala.scalajs.js.annotation.JSName
import japgolly.scalajs.react._
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.annotation.JSImport
import evolution.compiler.ast.AST.Bool

object CodeMirror {

  @JSImport("codemirror/mode/clike/clike", JSImport.Namespace)
  @js.native
  object CLike extends js.Object

  CLike

  @JSImport("react-codemirror2", "UnControlled")
  @js.native
  object RawComponent extends js.Object

  @js.native
  trait Editor extends js.Object

  @js.native
  trait Data extends js.Object

  @js.native
  trait Options extends js.Object {
    var theme: String
    var mode: String
    var tabSize: Int
    var lineNumbers: Boolean
  }

  object Options {
    def apply(theme: String, mode: String, tabSize: Int, lineNumbers: Boolean): Options = {
      val options = (new js.Object).asInstanceOf[Options]
      options.theme = theme
      options.mode = mode
      options.tabSize = tabSize
      options.lineNumbers = lineNumbers
      options
    }
  }

  type OnChange = js.Function3[Editor, Data, String, Unit]

  @js.native
  trait Props extends js.Object {
    var value: String = js.native
    var onChange: OnChange = js.native
    var options: Options = js.native
  }

  def props(value: String, onChange: (Editor, Data, String) => Callback): Props = {
    val p = (new js.Object).asInstanceOf[Props]
    p.value = value
    p.onChange = (editor: Editor, data: Data, value: String) => onChange(editor, data, value).runNow()
    p.options = Options(theme = "evolution", mode = "clike", tabSize = 2, lineNumbers = false)
    p
  }

  val component = JsComponent[Props, Children.Varargs, Null](RawComponent)
}
