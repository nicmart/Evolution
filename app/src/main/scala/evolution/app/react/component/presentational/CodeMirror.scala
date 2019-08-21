package evolution.app.react.component.presentational

import scalajs.js
import japgolly.scalajs.react._
import scala.scalajs.js.annotation.JSImport

object CodeMirror {

  @JSImport("codemirror/mode/clike/clike", JSImport.Namespace)
  @js.native
  object CLike extends js.Object
  CLike

  @JSImport("codemirror/addon/edit/matchbrackets", JSImport.Namespace)
  @js.native
  object MatchBrackets extends js.Object
  MatchBrackets

  @JSImport("codemirror/keymap/sublime", JSImport.Namespace)
  @js.native
  object Sublime extends js.Object
  Sublime

  @JSImport("react-codemirror2", "Controlled")
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
    var lineWrapping: Boolean
    var matchBrackets: Boolean
    var keyMap: String
  }

  // See https://github.com/scalacenter/scastie/blob/35f4f65647/client/src/main/scala/com.olegych.scastie.client/components/editor/EditorOptions.scala
  // for options used by Scastie
  object Options {
    def apply(
      theme: String,
      mode: String,
      tabSize: Int,
      lineNumbers: Boolean,
      lineWrapping: Boolean,
      matchBrackets: Boolean,
      keyMap: String
    ): Options = {
      val options = (new js.Object).asInstanceOf[Options]
      options.theme = theme
      options.mode = mode
      options.tabSize = tabSize
      options.lineNumbers = lineNumbers
      options.lineWrapping = lineWrapping
      options.matchBrackets = matchBrackets
      options.keyMap = keyMap
      options
    }
  }

  type OnChange = js.Function3[Editor, Data, String, Unit]
  type OnBeforeChange = js.Function3[Editor, Data, String, Unit]
  @js.native
  trait Props extends js.Object {
    var value: String = js.native
    var onChange: OnChange = js.native
    var onBeforeChange: OnBeforeChange = js.native
    var options: Options = js.native
  }

  def props(
    value: String,
    onChange: (Editor, Data, String) => Callback,
    onBeforeChange: (Editor, Data, String) => Callback
  ): Props = {
    val p = (new js.Object).asInstanceOf[Props]
    p.value = value
    p.onChange = (editor: Editor, data: Data, value: String) => onChange(editor, data, value).runNow()
    p.onBeforeChange = (editor: Editor, data: Data, value: String) => onBeforeChange(editor, data, value).runNow()
    p.options = Options(
      theme = "evolution",
      mode = "text/x-scala",
      tabSize = 2,
      lineNumbers = false,
      lineWrapping = false,
      matchBrackets = true,
      keyMap = "sublime"
    )
    p
  }

  val component = JsComponent[Props, Children.Varargs, Null](RawComponent)
}
