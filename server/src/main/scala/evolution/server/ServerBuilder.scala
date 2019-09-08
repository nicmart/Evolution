package evolution.server

import cats.effect._
import org.http4s.server.Server
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger

final class ServerBuilder[F[_]: ConcurrentEffect: Timer: ContextShift] {

  def server: Resource[F, Server[F]] =
    for {
      blocker <- Blocker[F]
      staticAssetsRoutes = new StaticAssetsRoutes[F](blocker, "/public/index-dev.html")
      httpApp = staticAssetsRoutes.routes.orNotFound
      loggedApp = Logger.httpApp(true, false)(httpApp)
      server <- BlazeServerBuilder[F].bindHttp(8080).withHttpApp(loggedApp).resource
    } yield server
}
