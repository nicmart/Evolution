package evolution.app.react.routing

import evolution.app.codec.Codec
import evolution.app.react.component.App
import evolution.app.react.pages._
import evolution.app.react.underware.SnapshotUnderware
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.router.StaticDsl.RouteB
import japgolly.scalajs.react.extra.router.{ BaseUrl, Redirect, RouterConfig, RouterConfigDsl }
import japgolly.scalajs.react.vdom.html_<^._

class Routing(
  urlDelimiter: String,
  appComponent: App.ReactComponent,
  defaultPage: MyPages,
  pageStateCodec: Codec[LoadDrawingPage, DrawingPageUrl]
) {

  private val drawingStateCodec: Codec[LoadDrawingPage, String] =
    pageStateCodec >> new Codec[DrawingPageUrl, String] {
      def decode(r: String): Option[DrawingPageUrl] = Some(DrawingPageUrl(r, ""))
      def encode(t: DrawingPageUrl): String = t.drawingSegment
    }

  val baseUrl: BaseUrl =
    BaseUrl.until(urlDelimiter)

  val config: RouterConfig[MyPages] = RouterConfigDsl[MyPages].buildConfig { dsl =>
    import dsl._

    (emptyRule
      | HomePageRoute(dsl).rule
      | (emptyRule
        | LoadDrawingPageRoute(dsl).rule
        | NotFoundPageRoute(dsl).rule).prefixPath(s"$urlDelimiter/")).notFound(redirectToPage(NotFound)(Redirect.Push))
  }

  private case class HomePageRoute(dsl: RouterConfigDsl[MyPages]) {
    import dsl._

    val rule: dsl.Rule =
      route ~> renderPage

    private def route =
      staticRoute(root, Home)

    private def renderPage =
      redirectToPage(defaultPage)(Redirect.Replace)
  }

  private case class LoadDrawingPageRoute(dsl: RouterConfigDsl[MyPages]) {
    import dsl._

    val rule: dsl.Rule =
      route ~> renderPage

    private def route =
      dynamicRouteCT[LoadDrawingPage](routeFromCodec(string(".*"), drawingStateCodec))

    private def renderPage: LoadDrawingPage => dsl.Renderer =
      dsl.dynRenderR { (loadDrawingPage, router) =>
        appComponent(
          SnapshotUnderware.simpleSnapshot[PageState](loadDrawingPage.state)(
            pageState =>
              Callback(println("PageStateSnapshot Callback called")) >> router.set(LoadDrawingPage(pageState))
          )
        )
      }
  }

  private case class NotFoundPageRoute(dsl: RouterConfigDsl[MyPages]) {
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

final case class DrawingPageUrl(drawingSegment: String, materializerSegment: String)
