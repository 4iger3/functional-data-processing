package typedjson

import io.circe.Json
import java.time.Instant

object StructuredLog:
  def event(level: String, name: String, fields: (String, Json)*): Unit =
    val record = Json.obj(
      (Vector(
        "timestamp" -> Json.fromString(Instant.now().toString),
        "level" -> Json.fromString(level),
        "event" -> Json.fromString(name)
      ) ++ fields)*
    )
    Console.err.println(record.noSpaces)

  def enabled(configured: String, level: String): Boolean =
    val order = Map("debug" -> 0, "info" -> 1, "warn" -> 2, "error" -> 3, "off" -> 4)
    order.get(configured).exists(order.getOrElse(level, 4) >= _)
