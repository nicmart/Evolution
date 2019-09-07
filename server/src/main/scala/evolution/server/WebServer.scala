package evolution.server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

import scala.concurrent.{ Future, Promise }
import scala.io.StdIn

object WebServer {
  def main(args: Array[String]) {

    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    val port = 12345

    val route =
      pathSingleSlash {
        getFromResource("public/index-dev.html")
      } ~
        pathPrefix(Remaining) { file =>
          // optionally compresses the response with Gzip or Deflate
          // if the client accepts compressed responses
          encodeResponse {
            getFromResource("public/" + file)
          }
        }

    val bindingFuture = Http().bindAndHandle(route, "localhost", port)

    def shutdown = {
      val promise = Promise[Unit]()
      sys.addShutdownHook {
        promise.success(())
      }
      Future {
        if (StdIn.readLine(s"Server online at http://localhost:$port/\nPress RETURN to stop...") != null)
          promise.success(())
      }
      promise.future
    }

    shutdown.onComplete { _ =>
      bindingFuture
        .flatMap(_.unbind()) // trigger unbinding from the port
        .onComplete(_ => system.terminate()) // and shutdown when done
    }
  }
}
