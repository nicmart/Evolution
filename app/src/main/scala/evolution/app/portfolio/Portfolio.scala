package evolution.app.portfolio
import evolution.app.model.Drawing
import evolution.app.model.state.DrawingState
import evolution.app.conf.Conf.defaultRendererState
import evolution.app.model.DrawingRepository
import cats.implicits._
import cats.effect.IO
import evolution.app.model.state.InfiniteCanvas
import evolution.app.model.state.TrailSettings

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
    ),
    Drawing(
      Some("Doodles"),
      DrawingState(
        0L,
        """
        |rx = 0.1 in
        |ax = 0 in
        |bx = -0.1 in
        |ry = 0.1 in
        |ay = -0.001 in
        |by = -0.001 in
        |length = 10000 in
        |
        |equation = r -> a -> b -> map(uniform(-1, 1), rnd -> z -> v -> r * rnd + a * z + b * v) in
        |
        |randomPoints = @point(uniform(left, right), uniform(bottom, top)) in
        |
        |flatMap(
        |  randomPoints,
        |  p -> take(
        |    length,
        |    const(p) + @point(
        |      solve2(equation(rx, ax, bx), 0, 0),
        |      solve2(equation(ry, ay, by), 0, 0)
        |    )
        |  )
        |)
        |
        |""".stripMargin
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Circular doodle"),
      DrawingState(
        0L,
        """
        |radius = .4  in
        |viscosity = 0.0003 in
        |k = .6 in
        |step = 2 in
        |
        |rnd = @point(uniform(-radius, radius), uniform(-radius, radius)) in
        |v = solve2(map(rnd, r -> x -> v -> r + -viscosity * v + -k * x), point(0, 0), point(0, 0)) in
        |
        |integrate(point(0, 0), step * v)
        |""".stripMargin
      ),
      defaultRendererState
    ),
    Drawing(
      Some("Bubbles"),
      DrawingState(
        0L,
        """
        |total = 1500 in
        |
        |circle = r -> w -> @polar(const(r), integrate(0, const(w))) in
        |rndPoint = take(total, @point(uniform(left, right), uniform(bottom, top))) in
        |radiuses = uniform(1, 30) in
        |
        |flatMap(
        |  rndPoint,
        |  p -> take(100, flatMap(radiuses, r -> map(circle(r, .1), p2 -> p + p2)))
        |)""".stripMargin
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Tenie"),
      DrawingState(
        0L,
        """
        |viscosity = .001 in
        |k = 0 in
        |randomForceStrength = .1 in
        |orthogonalLineLength = 50 in
        |orthogonalFactor = .3 in
        |
        |randomForces = @point(
        |  uniform(-randomForceStrength, randomForceStrength),
        |  uniform(-randomForceStrength, randomForceStrength)
        |) in
        |
        |acceleration = r -> x -> v -> r -viscosity * v + k * x in
        |
        |line = solve2(
        |  map(randomForces, acceleration),
        |  point(0, 0),
        |  point(0, 0)
        |) in
        |
        |flatten(mapWithDerivative(
        |  p -> v ->
        |    rotated = orthogonalFactor * point(y(v), -x(v)) in
        |    start = p - (toDbl(orthogonalLineLength)/2) * rotated in
        |    integrate(start, take(orthogonalLineLength, const(rotated))),
        |  line
        |))""".stripMargin
      ),
      defaultRendererState
    ),
    Drawing(
      Some("Mountains"),
      DrawingState(
        0L,
        """
        |a = 300 in
        |xFreq = 300 in
        |octaves = 8 in
        |persistence = .2 in
        |v = 2 in
        |length = floor((right - left) / v) in
        |s = 300 in // distance of the screen
        |cameraZ = 1000 in // height of the camera
        |offsetZ = cameraZ / 2 in
        |noiseStrength = 100 in // noise amplitude
        |maxDepth = 1000 in
        |depthStep = 3 in
        |startY = s/2 in
        |
        |fz = withFirst2(octaveNoise, on1 -> on2 ->
        |  const(x -> y ->
        |    -cameraZ + noiseStrength
        |     * on1(octaves, persistence, point(x/xFreq, 10* on2(16, .4, point(x/400, y/200))))
        |  )
        |) in
        |
        |
        |withFirst(fz, f ->
        |  flatMap(
        |    range(startY, maxDepth, depthStep),
        |    y ->
        |      xlength = (y/startY) * 2 * toDbl(length) / v in
        |      take(
        |        floor(xlength),
        |        map(
        |          integrate(-xlength, const(v)),
        |          x -> point(s * x / y, offsetZ + s * f(x, y) / y)
        |        )
        |      )
        |  )
        |)""".stripMargin
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Noise Vector field on grid"),
      DrawingState(
        0L,
        """
        |speed = 2 in
        |length = 15 in
        |gridSize = 40 in
        |noiseStrength = 3 in
        |freq = 0.003 in
        |
        |grid = gridSize -> flatMap(
        |  range(bottom, top, gridSize),
        |  y -> map(
        |    range(left, right, gridSize),
        |    x -> point(x, y)
        |  )
        |) in
        |
        |vectorFields = map(
        |  noise,
        |  n -> p -> polar(speed, noiseStrength * n(freq * p))
        |) in
        |
        |segment = v -> p ->
        |  take(
        |    length,
        |    integrate(-.5 * length * v + p, const(v))
        |  )
        |in
        |
        |
        |withFirst( 
        |  vectorFields,
        |  vectorField ->
        |    parallel(map(
        |      grid(gridSize),
        |      p -> segment(vectorField(p), p)
        |    ))
        |)""".stripMargin
      ),
      defaultRendererState.copy(strokeSize = 5)
    ),
    Drawing(
      Some("Orbiting particles"),
      DrawingState(
        0L,
        """
        |g = 100 in
        |k = .01 in
        |field = x -> v -> -(g/norm(x)^2) * versor(x) in
        |
        |parallel(take(1000,
        |
        |  zipWith(
        |    @point(uniform(left, right), uniform(bottom, top)),
        |    uniform(0, 2 * pi),
        |    uniform(0, 1),
        |    p -> alpha -> r ->
        |      solve2(
        |        const(field),
        |        p,
        |        polar(r * k * norm(p), alpha)
        |      )
        |  )
        |
        |))""".stripMargin
      ),
      defaultRendererWithInfiniteCanvas.copy(strokeSize = 5, iterations = 5000, trail = TrailSettings(true, 0.12))
    )
  )

  private lazy val defaultRendererWithInfiniteCanvas = defaultRendererState.copy(offCanvasSettings = InfiniteCanvas)

  def loadIntoRepository(repository: DrawingRepository): IO[Unit] =
    drawings.zipWithIndex.traverse { case (d, i) => repository.save(i.toString, d) }.void
}
