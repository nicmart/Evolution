package evolution.app.portfolio
import evolution.app.model.Drawing
import evolution.app.model.state.DrawingState
import evolution.app.conf.Conf.defaultRendererState
import evolution.app.model.DrawingRepository
import cats.implicits._
import cats.effect.IO

object Portfolio {
  val drawings = List(
    Drawing(
      Some("constant"),
      DrawingState(0L, "const(point(0, 0))"),
      defaultRendererState
    ),
    Drawing(
      Some("constant2"),
      DrawingState(0L, "const(point(100, 100))"),
      defaultRendererState
    ),
    Drawing(
      Some("brownian"),
      DrawingState(0L, """
      |r = uniform(-2, 2) in
      |integrate(point(0, 0), @point(r, r))
      """.stripMargin),
      defaultRendererState
    )
  )

  def loadIntoRepository(repository: DrawingRepository): IO[Unit] =
    drawings.zipWithIndex.traverse { case (d, i) => repository.save(i.toString, d) }.void
}
