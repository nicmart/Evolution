package evolution.app

import evolution.app.conf.Conf
import evolution.app.react.component.PageComponent
import japgolly.scalajs.react.extra.router.{BaseUrl, Redirect, Router, RouterConfigDsl}
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import evolution.app.react.pages._

object ReactApp {

  private val baseUrl =
    BaseUrl(BaseUrl.until_#.value)
  private val routerConfig = RouterConfigDsl[MyPages].buildConfig { dsl =>
    import dsl._

    val loadDrawingPagePath =
      string(".*").pmap[LoadDrawingPage] { string =>
        Conf.loadDrawingPageStringCodec.decode(string)
      } { page =>
        Conf.loadDrawingPageStringCodec.encode(page)
      }

    (emptyRule
      | staticRoute(root, Home) ~> redirectToPage(Conf.initialPage)(Redirect.Replace)
      | dynamicRoute[LoadDrawingPage]("#/" ~ loadDrawingPagePath) {
        case page @ LoadDrawingPage(_) => page
      } ~> dynRenderR((loadDrawingPage, router) =>
          PageComponent.component.apply(PageComponent.Props(router , loadDrawingPage.loadableDrawing))
        )
      | staticRoute("#notFound", NotFound) ~> render(<.div("NOT FOUND"))
      ).notFound(redirectToPage(NotFound)(Redirect.Push))
  }

  def main(args: Array[String]): Unit = {
    val router = Router(baseUrl, routerConfig)
    router().renderIntoDOM(dom.document.getElementById("entrypoint"))
  }
}
