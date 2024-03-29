package evolution.app.portfolio
import evolution.app.model.Drawing
import evolution.app.model.state.{DrawingState, OffCanvasStrategy, TrailSettings}
import evolution.app.conf.Conf.defaultRendererState

object Portfolio:
  val drawings = List(
    Drawing(
      Some("Constant Point"),
      DrawingState(0L, "const(point(0, 0))"),
      defaultRendererState
    ),
    Drawing(
      Some("Simple Brownian"),
      DrawingState(
        0L,
        """
        r = uniform(-2, 2) in
        integrate(point(0, 0), @point(r, r))
      """.unindent
      ),
      defaultRendererState
    ),
    Drawing(
      Some("Grid of concentric circles"),
      DrawingState(
        0L,
        """
        r = .03 in
        s = 100 in
        g = 100 in
        
        mid = point(g/2, g/2) in
        vectorField = p -> r * (-mid +point(y(p) % g, (-x(p)) % g)) in
        
        line(p) = solve1(const(vectorField), p) in
        
        flatMap(randomPoint, p -> line(p).take(s))
      """.unindent
      ),
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
          
          rndPoint = @point(uniform(left, right), uniform(bottom, top)) in
          
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
          speedMax = solve2(
            const(x -> v -> x),
            0,
            0
          ) in
          
          @point(speedMax, const(0))
          """.unindent
      ),
      defaultRendererState
    ),
    Drawing(
      Some("Oscillator drawn on brownian"),
      DrawingState(
        0L,
        """
          evo = randomWalk(2) in

          noiseStrength = @polar(bernoulli(.00003, 30), uniform(0, 2 * pi)) in
          
          oscillator =
            dampedOscillator(-1, -.0003, noiseStrength)
          in
          
          
          oscillator.drawOn(evo)
          """.unindent
      ),
      defaultRendererState
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
          
          flatMap(
            randomPoint,
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
      Some("Smoke"),
      DrawingState(
        0L,
        """
          radius = .4  in
          viscosity = 0.0003 in
          k = .6 in
          step = 2 in
          
          integrate(
            point(0, 0),
            step * dampedOscillator(-k, -viscosity, uniformPoint(radius))
          )
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
            line,
            p -> v ->
              rotated = orthogonalFactor * point(y(v), -x(v)) in
              start = p - (toDbl(orthogonalLineLength)/2) * rotated in
              integrate(start, const(rotated).take(orthogonalLineLength))
          ))""".unindent
      ),
      defaultRendererState
    ),
// This fails unification for some reason
//    Drawing(
//      Some("Mountains"),
//      DrawingState(
//        0L,
//        """
//          a = 300 in
//          xFreq = 300 in
//          octaves = 8 in
//          persistence = .2 in
//          v = 2 in
//          length = floor((right - left) / v) in
//          s = 300 in // distance of the screen
//          cameraZ = 1000 in // height of the camera
//          offsetZ = cameraZ / 2 in
//          noiseStrength = 100 in // noise amplitude
//          maxDepth = 1000 in
//          depthStep = 3 in
//          startY = s/2 in
//
//          on1 <- octaveNoises in
//          on2 <- octaveNoises in
//
//          f(x, y) = -cameraZ + noiseStrength * on1(
//                 octaves,
//                 persistence,
//                 point(
//                   x/xFreq,
//                   10* on2(16, .4, point(x/400, y/200))
//                 )
//               )
//          in
//
//
//          flatMap(
//            range(startY, maxDepth, depthStep),
//            y ->
//              xlength = length * (y/startY) * 2 / v in
//                map(
//                  integrate(-xlength, const(v)),
//                  x -> point(s * x / y, offsetZ + s * f(x, y) / y)
//                ).take(xlength.floor)
//          )
//          """.unindent
//      ),
//      defaultRendererWithInfiniteCanvas
//    ),
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
          
          noise <- noises in
          
          vectorField(p) = polar(speed, noiseStrength * noise(freq * p)) in
          
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
          noise <- noises in
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
          
          n(r, alpha) = r + noiseStrength * smoothNoise(alpha) * noise(polar(sr + kr * r, alpha)) in
          
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
    ),
    Drawing(
      Some("Rectangles"),
      DrawingState(
        0L,
        """
        d = 1 in

        hrects(n, f, l, r, b, t) =
          orderedUniformDiscreteWithEndpoints(l, r, d, n)
            .slidingMap(x1 -> x2 -> f(x1, x2, b, t))
            .flatten
        in
        
        vrects(n, f, l, r, b, t) =
          orderedUniformDiscreteWithEndpoints(b, t, d, n)
            .slidingMap(y1 -> y2 -> f(l, r, y1, y2))
            .flatten
        in
        
        rect(x1, x2, y1, y2) = [
            line(point(x1, y1), point(x1, y2), 1),
            line(point(x1, y1), point(x2, y1), 1)
        ].flatten in
        
        
        g(s, f) = vrects(s, hrects(s, f)) in
        
        dd = iterateFunc(g(5), 3, rect) in
        
        dd(left, right, bottom, top)
      """.unindent
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Rectangles with gradients"),
      DrawingState(
        0L,
        """
          k = .3 in

          hline(p1, p2, u) = line(point(u, p1.y), point(u, p2.y), 1) in
          vline(p1, p2, u) = line(point(p1.x, u), point(p2.x, u), 1) in
          
          lines(draw, p1, p2, us) =
            product(u <- us, p <- draw(p1, p2, u)) in p
          in
          
          hlines(p1, p2, g) = lines(hline, p1, p2, g(p1.x, p2.x)) in
          vlines(p1, p2, g) = lines(vline, p1, p2, g(p1.y, p2.y)) in
          
          gradients = uniformChoice(
            gradient(.4, .6),
            gradient(.2, .6),
            gradient(1, .6),
            reversegradient(.2, .6),
            reversegradient(.4, .6),
            reversegradient(1, .6)
          ) in
          
          randomFilling(p1, p2) =
            lines <- uniformChoice(hlines, vlines) in
            g <- gradients in
            lines(p1, p2, g)
          in
          
          fill(p1, p2) =
            [rect(1, p1, p2), randomFilling(p1, p2)].flatten          
          in
          
          m = 6 in
          
          splitHorizontally(
            split(20),
            splitVertically(randomSplit(20, 1), padded(fill, 6)),
            bottomLeft,
            topRight
          )
      """.unindent
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Waves"),
      DrawingState(
        0L,
        """
        hspeed = 3 in
        vspeed = 1 in
        strength = 500 in
        scalex = 0 in
        scaley = .002 in
        
        strength2 = 0 in
        scale2 = 0.003 in
        m = 3 in
        
        noises2Ds(strength, scalex, scaley) = zip(
          noisex <- noises,
          noisey <- noises
        ) in p -> strength * point(noisex(scalex * p), noisey(scaley * p))
        in
        
        perturbations2Ds(strength, scalex, scaley) = 
          noises2Ds(strength, scalex, scaley).map(noise -> p -> p + noise(p))
        in
        
        pert <- perturbations2Ds(strength, scalex, scaley) in
        pertUp <- noises2D(strength2, scale2).map(n -> p -> p + point(n(p).x, m + abs(n(p).y))) in
        pertDown <- noises2D(strength2, scale2).map(n -> p -> p + point(n(p).x, -m -abs(n(p).y))) in 
        
        perturbedLine(yy, offset1, offset2) =
          width = left - right in
          speed = hspeed * (offset1 + offset2 + width) / width in
          line(point(left + offset1, yy), point(right - offset2, yy), speed)
            .map(p -> point(p.x, pert(p).y))
        in
        
        connect(points) = points.slidingMap(p1 -> p2 -> line(pertUp(p1), pertDown(p2), vspeed)).flatten in
        
        d = 15 in
        n = floor(1.5 * (top - bottom) / d) + 1 in
        r = 100 in
        
        lines = zip(
          y <- range(1.5 * bottom, 1.5 * top, d).take(n),
          offset1 <- uniform(-r, 0),
          offset2 <- uniform(-r, 0)
        ) in perturbedLine(y, offset1, offset2)
        in
        
        lines
          .parallel
          .grouped(n)
          .flatMap(lst -> connect(fromList(lst)))
      """.unindent
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Windows on the Universe"),
      DrawingState(
        0L,
        """
        dist = 20 in
        noiseStrength = 60 in
        noiseScale = .006 in
        padding = 5 in

        randomWalks(p1, p2) =
          (randomWalk(6) + .5 * p1).take(50000).filter(inRect(p1, p2))
        in

        tile(p1, p2) =
          [
            randomWalks(p1, p2),
            rect(1, p1, p2)
          ].flatten
        in

        splitVertically(
          split(30),
          splitHorizontally(
            randomSplit(5, 10),
            padded(tile, padding)
          ),
          bottomLeft,
          topRight
        )
      """.unindent
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Webified Random Walk"),
      DrawingState(
        0L,
        """
        d = 10 in
        r = 80 in
        length = 100000 in
        
        webify(evo) =
          f <- evo.parametrizations(length) in
          integrate(0, uniform(-r, r)).slidingMap(
            t1 -> t2 -> line(f(t1), f(t2), 10)
          ).flatten
        in
        
        
        integrate(point(0, 0), @point(uniform(-d, d), uniform(-d, d))).webify
        """.unindent
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Clifford"),
      DrawingState(
        0L,
        """
        scale = 500 in
        scaleInv = .002 in
        a = -1.4 * scaleInv  in
        b = 1.6 * scaleInv in
        c = 1.1 in
        d = 0.7  in
        
        rnd = @point(uniform(left, right), uniform(bottom, top)) in
        trajectory(p) = 
          solve1(
            const(p -> -p + scale * point(
              sin(a * y(p)) + c * cos(a * x(p)),
              sin(b * x(p)) + d * cos(b * y(p))
            )),
            p 
          )
        in
           
        trajectory(point(0, 0))
        """.unindent
      ),
      defaultRendererWithInfiniteCanvas
    ),
    Drawing(
      Some("Double speed random walk"),
      DrawingState(
        0L,
        """
        rnd(r, n) =
            total <- uniform(1, n) in
            @point(uniform(-r, r), uniform(-r, r)).take(total.floor)
        in
        v1 = rnd(2, 3000) in
        v2 = rnd(10, 300) in
        
        integrate(point(0, 0), uniformFrom(2, [v1, v2]).flatten) 
        """.unindent
      ),
      defaultRendererState.copy(offCanvasSettings = OffCanvasStrategy.Torus)
    ),
    Drawing(
      Some("Squared random walks on a grid"),
      DrawingState(
        0L,
        """
        gridSize = 100 in
        l = 100 in
        step = 10 in
        directions = uniformFrom(4, [point(1, 0), point(0, 1), point(-1, 0), point(0, -1)]) in
        vs = directions.flatMap(p -> const(p).take(step)) in
        evo = integrate(origin, vs).take(l) in 
             
        randomPointInGrid(gridSize).flatMap(p -> evo.symmetric(4) + p) 
        """.unindent
      ),
      defaultRendererState
    ),
    Drawing(
      Some("Sum of circles"),
      DrawingState(
        0L,
        """
        // r-vars denote the radius of the circle
        // w-vars the angular speed
        r1 = 300 in
        w1 = .001 in
        r2 = 100 in
        w2 = .002 in
        r3 = 100 in
        w3 = 1 in
        r4 = 10 in
        w4 = .008 in
        r5 = 100 in
        w5 = .008 in
        
        circle(r1, w1) +
        circle(r2, w2) +
        circle(r3, w3) +
        circle(r4, w4) +
        circle(r5, w5)
        """.unindent
      ),
      defaultRendererState
    ),
    Drawing(
      Some("Vortex"),
      DrawingState(
        0L,
        """
        a = -.000001 in
        k = 0.01 in
        
        trajectory(p) =
          solve1(
            const(q -> k * point(
               -y(q) + a * x(q) * norm(q)^2,
               x(q) + a * y(q) * norm(q)^2
            )),
            p
          )
        in
        
        randomPoint.flatMap(p -> trajectory(p).take(100))
        """.unindent
      ),
      defaultRendererState
    )
  )

  private lazy val defaultRendererWithInfiniteCanvas =
    defaultRendererState.copy(offCanvasSettings = OffCanvasStrategy.Infinite)

  extension (string: String)
    def unindent: String =
      val s = string.dropWhile(_ == '\n')
      val indent = s.takeWhile(_ == ' ')
      s.replaceAll(s"\n$indent", "\n").trim
