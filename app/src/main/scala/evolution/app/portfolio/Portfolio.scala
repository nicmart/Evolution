package evolution.app.portfolio
import evolution.app.model.Drawing
import evolution.app.model.state.DrawingState
import evolution.app.conf.Conf.defaultRendererState
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
        r = uniform(-2, 2) in
        integrate(point(0, 0), @point(r, r))
      """.unindent),
      defaultRendererState
    ),
    Drawing(
      Some("Symmetric Brownian"),
      DrawingState(
        0L,
        """
        r = 3 in
        v = @point(uniform(-r, r), uniform(-r, r)) in
        
        symm = e -> flatMap(e, p ->
          cons(p, cons(-p, cons(point(-x(p), y(p)), cons(point(x(p), -y(p)), empty))))
        ) in
        
        symm(integrate(point(0, 0), v))
      """.unindent
      ),
      defaultRendererState
    ),
    Drawing(
      Some("Normal Jumps"),
      DrawingState(
        0L,
        """
          start = point(left, 0) in
          jump = 150 in
          r = 5 in
          total = floor((right - left) / jump) in
          
          flatten(
            const(
              take(
                integrate(
                  start,
                  const(point(jump, 0)) + @point(normal(0, r), normal(0, r))
                ),
                total
              )
            )
          )""".unindent
      ),
      defaultRendererState
    ),
    Drawing(
      Some("Differential Equation: x' = sin(x)"),
      DrawingState(
        0L,
        """
          l = 10 in
          hSpeed = 2 in
          w = 0.01 in
          amplitude = 10 in
          
          rndPoint =@point(uniform(left, right), uniform(bottom, top)) in
          
          trajectory = p -> @point(
            integrate(x(p), const(hSpeed)),
            solve1(const(z -> amplitude * sin(w * z)), y(p))
          ) in 
          
          flatMap(
            rndPoint,
            p -> trajectory(p).take(l)
          )
          """.unindent
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Second Order Brownian"),
      DrawingState(
        0L,
        """
          l = 0.2 in
          f = 0.01 in
          
          rndPoint = @point(uniform(-l, l), uniform(-l, l)) in
          solve2(
            map(rndPoint, r -> x -> v -> r -f * v),
            point(0, 0),
            point(0, 0)
          )
          """.unindent
      ),
      defaultRendererState
    ),
    Drawing(
      Some("Brownian with puslsating speed"),
      DrawingState(
        0L,
        """
          r = .03 in
          noise = uniform(-r, r) in
          a = .1 in
          b = -.00001 in
          c = -.01 in
          
          eq(r, x, v) = a * r + b * x + c * v in
          
          speedMax = solve2(
            map(noise, eq),
            0,
            0
          ) in
          
          speed = product(
            R <- map(speedMax, s -> abs(s)),
            r1 <- uniform(-R, R).take(1),
            r2 <- uniform(-R, R).take(1)
          ) in point(r1, r2)
          in
          
          integrate(
            point(0, 0),
            speed
          )
          """.unindent
      ),
      defaultRendererState
    ),
    Drawing(
      Some("Comet"),
      DrawingState(
        0L,
        """
          flatten(
            const(
              solve2(
                map(@point(uniform(-0.8, 0.1), uniform(-0.8, 0.5)), r -> x -> v -> r + -0.01 * v),
                point(0, 0),
                point(0, 0)
              ).take(200)
            )
          )
          """.unindent
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Doodles"),
      DrawingState(
        0L,
        """
          rx = 0.1 in
          ax = 0 in
          bx = -0.1 in
          ry = 0.1 in
          ay = -0.001 in
          by = -0.001 in
          length = 10000 in
          
          equation(r, a, b) = map(uniform(-1, 1), rnd -> z -> v -> r * rnd + a * z + b * v) in
          
          randomPoints = @point(uniform(left, right), uniform(bottom, top)) in
          
          flatMap(
            randomPoints,
            p ->
              p + @point(
                solve2(equation(rx, ax, bx), 0, 0),
                solve2(equation(ry, ay, by), 0, 0)
              ).take(length)
          )
          """.unindent
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Circular doodle"),
      DrawingState(
        0L,
        """
          radius = .4  in
          viscosity = 0.0003 in
          k = .6 in
          step = 2 in
          
          rnd = @point(
            uniform(-radius, radius),
            uniform(-radius, radius)
          ) in
          
          acceleration(r, x, v) = r -viscosity * v -k * x in
          
          v = solve2(
            map(rnd, acceleration),
            point(0, 0),
            point(0, 0)
          ) in
          
          integrate(point(0, 0), step * v)
          """.unindent
      ),
      defaultRendererState
    ),
    Drawing(
      Some("Bubbles"),
      DrawingState(
        0L,
        """
          total = 1500 in
          
          rndPoint = randomPoint.take(total) in
          radiuses = uniform(1, 30) in
          
          flatMap(
            rndPoint,
            p -> flatMap(radiuses, r -> map(circle(r, .1), p2 -> p + p2)).take(100)
          )""".unindent
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Tenie"),
      DrawingState(
        0L,
        """
          viscosity = .001 in
          k = 0 in
          randomForceStrength = .1 in
          orthogonalLineLength = 50 in
          orthogonalFactor = .3 in
          
          randomForces = @point(
            uniform(-randomForceStrength, randomForceStrength),
            uniform(-randomForceStrength, randomForceStrength)
          ) in
          
          acceleration(r, x, v) = r -viscosity * v + k * x in
          
          line = solve2(
            map(randomForces, acceleration),
            point(0, 0),
            point(0, 0)
          ) in
          
          flatten(mapWithDerivative(
            p -> v ->
              rotated = orthogonalFactor * point(y(v), -x(v)) in
              start = p - (toDbl(orthogonalLineLength)/2) * rotated in
              integrate(start, const(rotated).take(orthogonalLineLength)),
            line
          ))""".unindent
      ),
      defaultRendererState
    ),
    Drawing(
      Some("Mountains"),
      DrawingState(
        0L,
        """
          a = 300 in
          xFreq = 300 in
          octaves = 8 in
          persistence = .2 in
          v = 2 in
          length = floor((right - left) / v) in
          s = 300 in // distance of the screen
          cameraZ = 1000 in // height of the camera
          offsetZ = cameraZ / 2 in
          noiseStrength = 100 in // noise amplitude
          maxDepth = 1000 in
          depthStep = 3 in
          startY = s/2 in
          
          on1 <- octaveNoise in
          on2 <- octaveNoise in
          
          f(x, y) = -cameraZ + noiseStrength * on1(
                 octaves,
                 persistence,
                 point(
                   x/xFreq,
                   10* on2(16, .4, point(x/400, y/200))
                 )
               )
          in
          
          
          flatMap(
            range(startY, maxDepth, depthStep),
            y ->
              xlength = length * (y/startY) * 2 / v in
                map(
                  integrate(-xlength, const(v)),
                  x -> point(s * x / y, offsetZ + s * f(x, y) / y)
                ).take(xlength.floor)
          )
          """.unindent
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Noise Vector field on grid"),
      DrawingState(
        0L,
        """
          speed = 2 in
          length = 15 in
          gridSize = 40 in
          noiseStrength = 3 in
          freq = 0.003 in
          
          n <- noise in
          
          vectorField(p) = polar(speed, noiseStrength * n(freq * p)) in
          
          segment = v -> p ->
            integrate(-.5 * length * v + p, const(v)).take(length)
          in
          
          
          parallel(map(
            grid(gridSize),
            p -> segment(vectorField(p), p)
          ))""".unindent
      ),
      defaultRendererState.copy(strokeSize = 5)
    ),
    Drawing(
      Some("Orbiting particles"),
      DrawingState(
        0L,
        """
          g = 100 in
          k = .01 in
          field(x, v) = -g/norm(x)^2 * versor(x) in
          
          particle(p, alpha, r) = solve2(
            const(field),
            p,
            polar(r * k * norm(p), alpha)
          ) in
          
          particles = zip(
            p <- randomPoint,
            alpha <- uniform(0, 2 * pi),
            r <- uniform(0, 1)
          ) in particle(p, alpha, r)
          in
          
          parallel(particles.take(1000))
        """.unindent
      ),
      defaultRendererWithInfiniteCanvas.copy(strokeSize = 5, iterations = 5000, trail = TrailSettings(true, 0.12))
    ),
    Drawing(
      Some("Serpinski triangle"),
      DrawingState(
        0L,
        """
          p1 = point(0, top) in
          p2 = point(left, bottom) in
          p3 = point(right, bottom) in
          
          f1(p) = 0.5 * (p + p1) in
          f2(p) = 0.5 * (p + p2) in
          f3(p) = 0.5 * (p + p3) in
          
          functions = uniformChoice(f1, f2, f3) in
          
          roll(
            functions,
            point(0, 0)
          )
        """.unindent
      ),
      defaultRendererState
    ),
    Drawing(
      Some("Concentric Perturbed Circles"),
      DrawingState(
        0L,
        """
          noise1 <- noise in
          kr = .003 in
          sr = 5 in
          noiseStrength = 100 in
          startAngle = 2 * pi in
          endAngle = 0 * pi in
          smoothlength = pi/2 in
          
          endAngleC = 2 * pi - endAngle in
          
          smoothNoise(alpha) =
            smoothStep(startAngle - smoothlength, startAngle, alpha) +
            smoothStep(endAngleC - smoothlength, endAngleC, 2*pi-alpha)
          in
          
          n(r, alpha) = r + noiseStrength * smoothNoise(alpha) * noise1(polar(sr + kr * r, alpha)) in
          
          circle(v, r) = map(
            integrate(0, const(v / r)).take(floor(2 * r * pi  / v)),
            alpha -> polar(n(r, alpha), alpha)
          ) in
          
          
          product(
            r <- range(200, 800, 50),
            p <- circle(1, r)  
          ) in p
        """.unindent
      ),
      defaultRendererWithInfiniteCanvas.copy(strokeSize = 5)
    ),
    Drawing(
      Some("Some Spirographs"),
      DrawingState(
        0L,
        """
          onPoints = points -> drawings -> length ->
            parallel(zipWith(
              points,
              drawings,
              p -> drawing -> map(drawing, q -> q + p).take(length)
            ))
          in
          
          circle = alpha -> r -> w ->
            @polar(const(r), integrate(alpha, const(w)))
          in
          
          r = 50 in
          
          circles = k ->
          
            zip(
              alpha <- uniform(0, 2 * pi),
              w1 <- uniformDiscrete(-3, 3, 1),
              w2 <- uniformDiscrete(-30, 30, 2),
              w3 <- uniformDiscrete(-30, 200, 10),
              w4 <- uniformDiscrete(-200, 30, 21),
              w5 <- uniformDiscrete(-30, 30, 5),
              w6 <- uniformDiscrete(-5, 5, 1),
              r1 <- uniformDiscrete(1, 50, .1),
              r2 <- uniformDiscrete(1, 10, .1),
              r3 <- uniformDiscrete(1, 30, .1),
              r4 <- uniformDiscrete(1, 30, .1),
              r5 <- uniformDiscrete(1, 10, .1),
              r6 <- uniformDiscrete(4, 50, 1)
            ) in
                sum = r1 + r2 + r3 + r4 + r5 + r6 in
          
                circle(alpha, r1 * r / sum, k * w1) +
                circle(alpha, r2 * r / sum, k * w2) +
                circle(alpha, r3 * r / sum, k * w3) +
                circle(alpha, r4 * r / sum, k * w4) +
                circle(alpha, r5 * r / sum, k * w5) +
                circle(alpha, r6 * r / sum, k * w6)
          in
          
          
          k = .01 in
          length = 20 in
          
          onPoints(grid(150), circles(k), floor(length / k))
        """.unindent
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Simple Spirographs"),
      DrawingState(
        0L,
        """
          onPoints(points, drawings, length) =
            flatten(
              zip(
                p <- points,
                drawing <- drawings
              ) in map(drawing, q -> q + p).take(length)
            )
          in
          
          circle(r, w) =
            @polar(const(r), integrate(0, const(w)))
          in
          
          w1s = uniformDiscrete(3, 7, 1) in
          w2s = uniformDiscrete(-5, -2, 1) in
          rs = uniformDiscrete(10, 30, 1) in
          
          mainCircles(k) = map(w1s, w -> circle(10, k * w)) in
          secondaryCircles(k) = zip(
            w <- w2s,
            r <- rs
          ) in circle(r, k * w)
          in
          
          circles(k) =
            zip(
              c1 <- mainCircles(k),
              c2 <- secondaryCircles(k)
            ) in c1 + c2
          in


          k = .03 in
          length = 20 in
          
          onPoints(grid(80), circles(k), floor(length / k))
        """.unindent
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("vertical multilines"),
      DrawingState(
        0L,
        """
        r = 30 in
        gridSize = 100 in
        delta = point(0, 100) in
        n = 100 in
        
        vertLine(p) =
          product(
            start <- p + uniformPoint(r).take(n),
            p <- line(start, start + delta, 1)
          ) in p
        in
        
        product(
          p <- randomPointInGrid(gridSize),
          q <- vertLine(p)
        ) in q
        """.unindent
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Connect dots on squares borders"),
      DrawingState(
        0L,
        """
        r = 50 in
        g = 50 in
        
        ps = randomPointOnSquareBorder(r, g).take(10) in
        
        lines =
          slidingMap(ps, s -> e -> line(s, e, 1)).flatten
        in
        
        flatten(grid(2 * r).map(p -> lines + p))
      """.unindent
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Square web"),
      DrawingState(
        0L,
        """
        g = 50 in
        l = 70 in
        r = integrate(0, uniform(-l, l)) in
        
        r = rectangleP(point(-g, -g), point(g, g), r) * .5 in
        
        d = r.slidingMap(
          p1 -> p2 -> line(p1, p2, 1)
        ).flatten in
        
        grid(g).map(p -> p + d).parallel
      """.unindent
      ),
      defaultRendererWithInfiniteCanvas
    )
  )

  private lazy val defaultRendererWithInfiniteCanvas = defaultRendererState.copy(offCanvasSettings = InfiniteCanvas)

  private implicit class StringOps(string: String) {
    def unindent: String = {
      //val indent = string.
      val s = string.dropWhile(_ == '\n')
      val indent = s.takeWhile(_ == ' ')
      s.replaceAll(s"\n$indent", "\n").trim
    }
  }
}
