package evolution.app

import evolution.app.react.component.PageComponent
import japgolly.scalajs.react.extra.router.{BaseUrl, Redirect, Router, RouterConfigDsl}
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

object ReactApp {

  sealed trait MyPages

  case object Home extends MyPages
  case object NotFound extends MyPages

  val baseUrl =
    BaseUrl(BaseUrl.until_#.value)
  val routerConfig = RouterConfigDsl[MyPages].buildConfig { dsl =>
    import dsl._

    (emptyRule
      | staticRoute(root, Home) ~> render(PageComponent.component.apply())
      | staticRoute("#notFound", NotFound) ~> render(<.div("NOT FOUND"))
      ).notFound(redirectToPage(NotFound)(Redirect.Push))
  }

  def main(args: Array[String]): Unit = {
    val router = Router(baseUrl, routerConfig)
    router().renderIntoDOM(dom.document.getElementById("entrypoint"))
  }
}
