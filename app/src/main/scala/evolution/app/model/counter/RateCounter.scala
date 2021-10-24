package evolution.app.model.counter

case class RateCounter(value: Int, last: RateValue, delta: Double):
  def count(additionalValue: Int): RateCounter =
    val currentTime = System.currentTimeMillis()
    if (currentTime - last.time < delta)
      copy(value = value + additionalValue)
    else
      val newValue = value + additionalValue
      RateCounter(0, last.next(newValue, currentTime.toDouble), delta)
  def rate: Double = last.rate

object RateCounter:
  def empty(delta: Double): RateCounter =
    RateCounter(0, RateValue(0, System.currentTimeMillis.toDouble), delta)

case class RateValue(rate: Double, time: Double):
  def next(value: Int, curTime: Double): RateValue =
    RateValue(1000 * value / (curTime - time), curTime)
