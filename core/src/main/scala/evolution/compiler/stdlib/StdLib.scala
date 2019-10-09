package evolution.compiler.stdlib

object StdLib {
  val code = """

origin = point(0, 0) in

grid(gridSize) = product(
  y <- range(top, bottom, -gridSize),
  x <- range(left, right, gridSize)
) in point(x, y)
in

randomPointInGrid(gridSize) = @point(
  uniformDiscrete(left, right, gridSize),
  uniformDiscrete(bottom, top, gridSize)
) in

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

line(from, to, step) =
  total = floor(norm(to - from) / step) + 1 in
  v = versor(to - from) in
  map(
    range(0, total, 1),
    s -> from + s * step * v
  )
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

rectangle(p1, p2, v) =
  p12 = point(p2.x, p1.y) in
	p21 = point(p1.x, p2.y) in
	w = abs(p2.x - p1.x) in
	h = abs(p2.y - p1.y) in
	l = 2 * (w + h) in
	tot = (l / v + 1) in
	range(0, l, v).map(
    u ->
      x1 = u in
    	y1 = u - w in
      x2 = u - w - h in
      y2 = u - 2 * w - h in
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
