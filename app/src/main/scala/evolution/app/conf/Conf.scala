package evolution.app.conf

import evolution.app.{CanvasInitializer, ColorCanvasInitializer, FullWindowCanvasInitializer}
import org.scalajs.dom

/**
  * Created by nic on 26/11/2016.
  */
object Conf {
    lazy val canvasInitializer: CanvasInitializer =
        ColorCanvasInitializer("black")

    lazy val drawings = ???
}
