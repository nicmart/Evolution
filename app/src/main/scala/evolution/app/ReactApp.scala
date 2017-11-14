package evolution.app

import evolution.app.react.component.PageComponent
import evolution.app.react.component.PageComponent.UrlState
import japgolly.scalajs.react.extra.router.{BaseUrl, Redirect, Router, RouterConfigDsl}
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

object ReactApp {

  sealed trait MyPages

  case object Home extends MyPages
  case class LoadDrawingPage(urlState: UrlState) extends MyPages
  case object NotFound extends MyPages

  private val baseUrl =
    BaseUrl(BaseUrl.until_#.value)
  private val routerConfig = RouterConfigDsl[MyPages].buildConfig { dsl =>
    import dsl._

    val loadDrawingPagePath =
      string(".*").pmap[LoadDrawingPage] { string =>
        UrlState.unserialize(string).map(LoadDrawingPage)
      } { page =>
        UrlState.serialize(page.urlState)
      }

    (emptyRule
      | staticRoute(root, Home) ~> renderR { router =>
        PageComponent.component.apply(PageComponent.Props(router , None))
      }
      | dynamicRoute[LoadDrawingPage]("#/" ~ loadDrawingPagePath) {
        case page @ LoadDrawingPage(_) => page
      } ~> dynRenderR((loadDrawingPage, router) =>
          PageComponent.component.apply(PageComponent.Props(router , Some(loadDrawingPage.urlState)))
        )
      | staticRoute("#notFound", NotFound) ~> render(<.div("NOT FOUND"))
      ).notFound(redirectToPage(NotFound)(Redirect.Push))
  }

  def main(args: Array[String]): Unit = {
    val router = Router(baseUrl, routerConfig)
    router().renderIntoDOM(dom.document.getElementById("entrypoint"))
  }
}
