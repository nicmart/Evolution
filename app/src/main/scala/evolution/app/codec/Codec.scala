package evolution.app.codec

trait Codec[T, R] {
  def encode(t: T): R
  def decode(r: R): Option[T]

  def >>[R2](codec: Codec[R, R2]): Codec[T, R2] =
    Codec.compose(this, codec)
}

object Codec {
  def instance[T, R](
    enc: T => R,
    dec: R => Option[T]
  ): Codec[T, R] =
    new Codec[T, R] {
      override def encode(t: T): R = enc(t)
      override def decode(r: R): Option[T] = dec(r)
    }

  def compose[T, R1, R2](
    codec1: Codec[T, R1],
    codec2: Codec[R1, R2]
  ): Codec[T, R2] =
    new Codec[T, R2] {
      override def encode(t: T): R2 =
        codec2.encode(codec1.encode(t))
      override def decode(r: R2): Option[T] =
        for {
          r1 <- codec2.decode(r)
          t <- codec1.decode(r1)
        } yield t
    }
}
