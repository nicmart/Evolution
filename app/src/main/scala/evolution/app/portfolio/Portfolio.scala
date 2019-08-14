package evolution.app.portfolio
import evolution.app.model.Drawing
import evolution.app.model.state.DrawingState
import evolution.app.conf.Conf.defaultRendererState
import evolution.app.model.DrawingRepository
import cats.implicits._
import cats.effect.IO
import evolution.app.model.state.OffCanvasStrategy
import evolution.app.model.state.InfiniteCanvas

object Portfolio {
  val drawings = List(
    Drawing(
      Some("Constant Point"),
      DrawingState(0L, "const(point(0, 0))"),
      defaultRendererState
    ),
    Drawing(
      Some("Simple Brownian"),
      DrawingState(0L, """
      |r = uniform(-2, 2) in
      |integrate(point(0, 0), @point(r, r))
      """.stripMargin),
      defaultRendererState
    ),
    Drawing(
      Some("Normal Jumps"),
      DrawingState(
        0L,
        """
        |start = point(left, 0) in
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
      Some("Differential Equation: x' = sin(x)"),
      DrawingState(
        0L,
        """
        |l = 10 in
        |hSpeed = 2 in
        |w = 0.01 in
        |amplitude = 10 in
        |
        |rndPoint =@point(uniform(left, right), uniform(bottom, top)) in
        |
        |trajectory = p -> @point(
        |  integrate(x(p), const(hSpeed)),
        |  solve1(const(z -> amplitude * sin(w * z)), y(p))
        |) in 
        |
        |flatMap(
        |  rndPoint,
        |  p -> take(
        |    l,
        |    trajectory(p)
        |  ) 
        |)
        |""".stripMargin
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Second Order Brownian"),
      DrawingState(
        0L,
        """
        |l = 0.2 in
        |f = 0.01 in
        |
        |rndPoint = @point(uniform(-l, l), uniform(-l, l)) in
        |solve2(
        |  map(rndPoint, r -> x -> v -> r -f * v),
        |  point(0, 0),
        |  point(0, 0)
        |)
        |""".stripMargin
      ),
      defaultRendererState
    ),
    Drawing(
      Some("Comet"),
      DrawingState(
        0L,
        """
        |flatten(
        |  const(
        |    take(
        |      200,
        |      solve2(
        |        map(@point(uniform(-0.8, 0.1), uniform(-0.8, 0.5)), r -> x -> v -> r + -0.01 * v),
        |        point(0, 0),
        |        point(0, 0)
        |      )
        |    )
        |  )
        |)
        |""".stripMargin
      ),
      defaultRendererWithInfiniteCanvas
    )
  )

  private lazy val defaultRendererWithInfiniteCanvas = defaultRendererState.copy(offCanvasSettings = InfiniteCanvas)

  def loadIntoRepository(repository: DrawingRepository): IO[Unit] =
    drawings.zipWithIndex.traverse { case (d, i) => repository.save(i.toString, d) }.void
}
