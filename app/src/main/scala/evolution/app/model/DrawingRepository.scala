package evolution.app.model
import scala.concurrent.Future

trait DrawingRepository {
  def save(id: String, drawing: Drawing): Future[Unit]
  def load(id: String): Future[Option[Drawing]]
}
