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
