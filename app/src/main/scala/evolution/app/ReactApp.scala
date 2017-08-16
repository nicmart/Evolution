package evolution.app

import evolution.app.react.component.{DrawingListComponent, NavbarComponent, PageComponent}
import japgolly.scalajs.react.extra.router.{BaseUrl, Redirect, Router, RouterConfigDsl}
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

import scala.scalajs.js.annotation.JSExport

object ReactApp {

  sealed trait MyPages

  case object Home extends MyPages

  val htmlPage = "/app/target/scala-2.12/classes/react.html"

  val baseUrl =
    BaseUrl(BaseUrl.fromWindowOrigin.value + htmlPage)
  val routerConfig = RouterConfigDsl[MyPages].buildConfig { dsl =>
    import dsl._

    (emptyRule
        | staticRoute(root, Home) ~> render(PageComponent.component())
        ).notFound(redirectToPage(Home)(Redirect.Replace))
  }

  def main(args: Array[String]): Unit = {
    val router = Router(baseUrl, routerConfig)
    router().renderIntoDOM(dom.document.getElementById("entrypoint"))
  }
}
