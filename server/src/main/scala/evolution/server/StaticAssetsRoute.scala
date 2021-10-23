package evolution.server

import cats.implicits._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.staticcontent._
import cats.effect._
import org.http4s._

final class StaticAssetsRoute(indexPath: String) {

  private val dsl = new Http4sDsl[IO] {}
  import dsl._

  val routes: HttpRoutes[IO] =
    indexRoute <+> filesRoute

  private lazy val indexRoute = HttpRoutes.of[IO] {
    case req @ GET -> Root =>
      StaticFile.fromResource(indexPath, Some(req)).getOrElseF(NotFound())
  }

  private lazy val filesRoute = ResourceServiceBuilder[IO]("/public").toRoutes
}
