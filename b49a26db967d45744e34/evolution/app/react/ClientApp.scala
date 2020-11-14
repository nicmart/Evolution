package evolution.app.react

import evolution.app.conf.Conf
import org.scalajs.dom

object ClientApp {

  def main(args: Array[String]): Unit = {
    Conf.router().renderIntoDOM(dom.document.getElementById("entrypoint"))
  }
}
