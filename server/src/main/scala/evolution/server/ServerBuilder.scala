package evolution.server

import cats.effect._
import org.http4s.server.Server
import org.http4s.implicits._
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.middleware.Logger

object ServerBuilder:

  def server: Resource[IO, Server] =
    for
      staticAssetsRoutes <- Resource.pure[IO, StaticAssetsRoute](StaticAssetsRoute("/public/index-dev.html"))
      httpApp = staticAssetsRoutes.routes.orNotFound
      loggedApp = Logger.httpApp(true, false)(httpApp)
      server <- BlazeServerBuilder[IO].bindLocal(8080).withHttpApp(loggedApp).resource
    yield server
