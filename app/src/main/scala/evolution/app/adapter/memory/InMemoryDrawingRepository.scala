package evolution.app.adapter.memory
import evolution.app.model.Drawing
import evolution.app.model.DrawingRepository
import cats.effect.IO
import cats.effect.concurrent.Ref

class InMemoryDrawingRepository(cache: Ref[IO, Map[String, Drawing]]) extends DrawingRepository {
  def save(id: String, drawing: Drawing): IO[Unit] =
    cache.update(_.updated(id, drawing))
  def load(id: String): IO[Option[Drawing]] = cache.get.map(_.get(id))
}
