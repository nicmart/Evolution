package evolution.app.codec

import java.io.{ ByteArrayOutputStream, ByteArrayInputStream }
import java.util.zip.{ GZIPOutputStream, GZIPInputStream }

import scala.util.Try

/**
 * @see <a href="https://gist.github.com/owainlewis/1e7d1e68a6818ee4d50e">
 */
object GzipCodec extends Codec[String, Array[Byte]] {

  override def encode(input: String): Array[Byte] = {
    val bos = new ByteArrayOutputStream(input.length)
    val gzip = new GZIPOutputStream(bos)
    gzip.write(input.getBytes())
    gzip.close()
    val compressed = bos.toByteArray
    bos.close()
    compressed
  }

  override def decode(compressed: Array[Byte]): Option[String] =
    Try {
      val inputStream = new GZIPInputStream(new ByteArrayInputStream(compressed))
      scala.io.Source.fromInputStream(inputStream).mkString
    }.toOption
}

object StringByteCodec extends Codec[String, Array[Byte]] {
  override def encode(t: String): Array[Byte] = t.getBytes()
  override def decode(r: Array[Byte]): Option[String] = Some(new String(r))
}
