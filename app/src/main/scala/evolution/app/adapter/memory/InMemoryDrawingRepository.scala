package evolution.app.adapter.memory
import scala.concurrent.Future
import evolution.app.model.Drawing
import evolution.app.model.DrawingRepository

class InMemoryDrawingRepository extends DrawingRepository {
  private var cache: Map[String, Drawing] = Map.empty
  def save(id: String, drawing: Drawing): Future[Unit] = Future.successful(cache = cache.updated(id, drawing))
  def load(id: String): Future[Option[Drawing]] = Future.successful(cache.get(id))
}
