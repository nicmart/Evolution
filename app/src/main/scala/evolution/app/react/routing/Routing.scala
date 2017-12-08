package evolution.app.react.routing

import evolution.app.codec.Codec
import evolution.app.react.component.App
import evolution.app.react.pages._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.extra.router.StaticDsl.RouteB
import japgolly.scalajs.react.extra.router.{BaseUrl, Redirect, RouterConfig, RouterConfigDsl, StaticDsl}
import japgolly.scalajs.react.vdom.html_<^._

class Routing[C](
  urlDelimiter: String,
  appComponent: App.ReactComponent[C],
  defaultPage: MyPages[C],
  drawingStateCodec: Codec[LoadDrawingPage[C], String]
) {
  val baseUrl: BaseUrl =
    BaseUrl.until(urlDelimiter)

  val config: RouterConfig[MyPages[C]] = RouterConfigDsl[MyPages[C]].buildConfig { dsl =>
    import dsl._

    (emptyRule
      | HomePageRoute(dsl).rule
      | (emptyRule
        | LoadDrawingPageRoute(dsl).rule
        | NotFoundPageRoute(dsl).rule
        ).prefixPath(s"$urlDelimiter/")
      ).notFound(redirectToPage(NotFound)(Redirect.Push))
  }

  private case class HomePageRoute(dsl: RouterConfigDsl[MyPages[C]]) {
    import dsl._

    val rule: dsl.Rule =
      route ~> renderPage

    private def route =
      staticRoute(root, Home)

    private def renderPage =
      redirectToPage(defaultPage)(Redirect.Replace)
  }

  private case class LoadDrawingPageRoute(dsl: RouterConfigDsl[MyPages[C]]) {
    import dsl._

    val rule: dsl.Rule =
      route ~> renderPage

    private def route =
      dynamicRouteCT[LoadDrawingPage[C]](routeFromCodec(string(".*"), drawingStateCodec))

    private def renderPage: LoadDrawingPage[C] => dsl.Renderer =
      dsl.dynRenderR { (loadDrawingPage, router) =>
        appComponent(
          StateSnapshot[PageState[C]](loadDrawingPage.state)(pageState => router.set(LoadDrawingPage(pageState)))
        )
      }
  }

  private case class NotFoundPageRoute(dsl: RouterConfigDsl[MyPages[C]]) {
    import dsl._

    val rule: dsl.Rule =
      route ~> render

    private def route =
      staticRoute("notFound", NotFound)

    private def render =
      dsl.render(<.div("NOT FOUND"))
  }

  private def routeFromCodec[T, R](path: RouteB[R], codec: Codec[T, R]): RouteB[T] =
    path.pmap(codec.decode)(codec.encode)
}