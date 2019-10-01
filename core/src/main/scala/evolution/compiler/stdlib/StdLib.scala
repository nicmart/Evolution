package evolution.compiler.stdlib

object StdLib {
  val code = """

grid(gridSize) = product(
    y <- range(top, bottom, -gridSize),
    x <- range(left, right, gridSize)
) in point(x, y)
in

randomPointInGrid(gridSize) = zip(
    x <- range(left, right, gridSize),
    y <- range(bottom, top, gridSize)
) in point(x, y)
in

line(from, to, step) =
    total = floor(norm(to - from) / step) + 1 in
    v = versor(to - from) in
    map(
        range(0, total, 1),
        s -> from + s * step * v
    )
in

export


"""
}
