package evolution.compiler.stdlib

object StdLib {
  val code = """

origin = point(0, 0) in

topLeft = point(left, top) in
topRight = point(right, top) in
bottomLeft = point(left, bottom) in
bottomRight = point(right, bottom) in 

grid(gridSize) = product(
  y <- range(top, bottom, -gridSize),
  x <- range(left, right, gridSize)
) in point(x, y)
in

randomPointInGrid(gridSize) = @point(
  uniformDiscrete(left, right, gridSize),
  uniformDiscrete(bottom, top, gridSize)
) in

copyOnGrid(gridSize, evolution) = 
  flatMap(
    evolution,
    p -> grid(gridSize).map(q -> p + q) // using overloaded "+" makes type ambiguous...
  )
in

replicateOnGrid(gridSize, evolution) =
  grid(gridSize).map(p -> evolution.map(q -> p + q)).parallel
in

randomPoint = @point(
  uniform(left, right),
  uniform(bottom, top)
) in

uniformPoint(r) = @point(
  uniform(-r, r),
  uniform(-r, r)
) in

randomPointOnSquareBorder(r, g) = 
	coord = uniformDiscrete(-r, r, g) in

  flatMap(
    uniformChoice(
      coord.map(x -> point(x, -r)),
      coord.map(x -> point(x, r)),
      coord.map(y -> point(-r, y)),
      coord.map(y -> point(r, y))
    ),
    ps -> ps.take(1)
  )
in

randomWalk(r) = integrate(point(0, 0), uniformPoint(r)) in

lineP(from, to, step, ts) =
  v = step * versor(to - from) in
  ts.map(t -> from + t * v)
in

line(from, to, step) =
  total = floor(norm(to - from) / step) + 1 in
  lineP(from, to, step, range(0, total, 1))
in

arcP(r, ts) = 
  ts.map(t -> polar(r, t / r))
in

arc(r, a1, a2, w) = map(
  range(a1, a2, w),
  a -> polar(r, a)
) in

finiteCircle(r, w) = arc(r, 0, 2 * pi, w) in

circle(r, w) = map(
  integrate(0, const(w)),
  a -> polar(r, a)
) in

rectangleP(p1, p2, ts) =
  p12 = point(p2.x, p1.y) in
	p21 = point(p1.x, p2.y) in
	w = abs(p2.x - p1.x) in
	h = abs(p2.y - p1.y) in
	l = 2 * (w + h) in
	ts.map(
    u ->
      uMod = u % l in
      x1 = uMod in
    	y1 = uMod - w in
      x2 = uMod - w - h in
      y2 = uMod - 2 * w - h in
    	if(
        x1 < w,
        (1 - x1/w) * p1 + (x1/w) * p12,
        if (
          y1 < h,
          (1 - y1/h) * p12 + (y1/h) * p2,
          if (
            x2 < w,
            (1 - x2/w) * p2 + (x2/w) * p21,
            (1 - y2/h) * p21 + (y2/h) * p1
        )
      )
  	)
  )
in

finiteRectangle(p1, p2, v) =
  rectangleP(p1, p2, range(0, 2 * (abs(p1.x - p2.x) + abs(p1.y - p2.y)), v)) in

rectangle(p1, p2, v) =
  rectangleP(p1, p2, iterate(x -> x + v, 0)) in

finiteSquare(r, v) = finiteRectangle(point(-r, -r), point(r, r), v) in
square(r, v) = rectangle(point(-r, -r), point(r, r), v) in

dampedOscillator(a, b, rnd) = 
  solve2(
    map(rnd, r -> x -> v -> r + a * x + b * v),
    point(0, 0),
    point(0, 0)
  )
in

bernoulli(p, r) = map(
  uniform(0, 1),
  sample -> if (sample < p, r, 0)
) in

export


"""
}
