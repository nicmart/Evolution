package evolution.compiler.stdlib

object StdLib {
  val code = """

grid(gridSize) = product(
    y <- range(top, bottom, -gridSize),
    x <- range(left, right, gridSize)
) in point(x, y)

in export


"""
}
