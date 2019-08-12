package evolution.app.portfolio
import evolution.app.model.Drawing
import evolution.app.model.state.DrawingState
import evolution.app.conf.Conf.defaultRendererState
import evolution.app.model.DrawingRepository
import scala.concurrent.Future

object Portfolio {
  val drawings = List(
    Drawing(
      Some("costant"),
      DrawingState(0L, "const(point(0, 0))"),
      defaultRendererState
    ),
    Drawing(
      Some("brownian"),
      DrawingState(0L, """
      |r = uniform(-2, 2) in
      |@point(r, r)
      """.stripMargin),
      defaultRendererState
    )
  )

  def load(repository: DrawingRepository): Future[Unit] =
    ???
}
