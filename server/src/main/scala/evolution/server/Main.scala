package evolution.server

import cats.effect.{ ExitCode, IO, IOApp }
import cats.implicits._

object Main extends IOApp {
  def run(args: List[String]) =
    new ServerBuilder[IO].server.use(_ => IO.never).as(ExitCode.Success)
}
