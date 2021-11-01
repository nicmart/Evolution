package evolution.app.data

case class PointedSeq[+T](elements: Seq[T], selected: T):
  def get(predicate: T => Boolean): T =
    elements.find(predicate).getOrElse(selected)
  def select[S >: T](element: S): PointedSeq[S] =
    PointedSeq(elements, element)
  def selectByPredicate(predicate: T => Boolean): PointedSeq[T] =
    select(get(predicate))
  def map[S](f: T => S): PointedSeq[S] =
    PointedSeq(elements.map(f), f(selected))
