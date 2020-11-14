package evolution.app

import io.circe.Json

package object codec {
  type JsonCodec[T] = Codec[T, Json]
}
