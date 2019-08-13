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
      Some("Constant Point"),
      DrawingState(0L, "const(point(0, 0))"),
      defaultRendererState
    ),
    Drawing(
      Some("Normal Jumps"),
      DrawingState(
        0L,
        """start = point(left, 0) in
        |jump = 150 in
        |r = 5 in
        |total = floor((right - left) / jump) in
        |
        |flatten(
        |  const(
        |    take(
        |      total,
        |      integrate(
        |        start,
        |        const(point(jump, 0)) + @point(normal(0, r), normal(0, r))
        |      )
        |    )
        |  )
        |)""".stripMargin
      ),
      defaultRendererState
    ),
    Drawing(
      Some("Simple Brownian"),
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
