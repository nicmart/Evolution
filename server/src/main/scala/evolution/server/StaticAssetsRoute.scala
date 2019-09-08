package evolution.server

import cats.effect._
import cats.implicits._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.staticcontent._
import cats.effect._
import org.http4s._

final class StaticAssetsRoutes[F[_]: Effect: ContextShift](blocker: Blocker, indexPath: String) {

  private val dsl = new Http4sDsl[F] {}
  import dsl._

  val routes: HttpRoutes[F] =
    indexRoute <+> filesRoute

  private lazy val indexRoute = HttpRoutes.of[F] {
    case req @ GET -> Root =>
      StaticFile.fromResource(indexPath, blocker, Some(req)).getOrElseF(NotFound())
  }

  private lazy val filesRoute = resourceService[F](ResourceService.Config("/public", blocker))
}
