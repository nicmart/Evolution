package evolution.server

import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp:
  def run(args: List[String]): IO[ExitCode] =
    ServerBuilder.server.use(_ => IO.never).as(ExitCode.Success)
