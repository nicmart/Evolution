package evolution.app.model
import cats.effect.IO

trait DrawingRepository {
  def save(id: String, drawing: Drawing): IO[Unit]
  def load(id: String): IO[Option[Drawing]]
}
