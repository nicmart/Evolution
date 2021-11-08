package evolution.server

import cats.implicits.*
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.staticcontent.*
import cats.effect.*
import org.http4s.*

final class StaticAssetsRoute(indexPath: String):

  private val dsl = new Http4sDsl[IO] {}
  import dsl.*

  val routes: HttpRoutes[IO] =
    indexRoute <+> filesRoute

  private lazy val indexRoute = HttpRoutes.of[IO] { case req @ GET -> Root =>
    StaticFile.fromResource(indexPath, Some(req)).getOrElseF(NotFound())
  }

  private lazy val filesRoute = ResourceServiceBuilder[IO]("/public").toRoutes
