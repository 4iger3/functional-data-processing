package typedjson

final case class JsonPath private (segments: Vector[JsonPath.Segment]):
  def field(name: String): JsonPath = copy(segments = segments :+ JsonPath.Field(name))
  def index(value: Long): JsonPath = copy(segments = segments :+ JsonPath.Index(value))

  override def toString: String =
    segments.foldLeft("$") {
      case (prefix, JsonPath.Field(name)) if name.matches("[A-Za-z_][A-Za-z0-9_]*") =>
        s"$prefix.$name"
      case (prefix, JsonPath.Field(name)) =>
        val escaped = name.replace("\\", "\\\\").replace("\"", "\\\"")
        s"""$prefix["$escaped"]"""
      case (prefix, JsonPath.Index(value)) => s"$prefix[$value]"
    }

object JsonPath:
  private[typedjson] sealed trait Segment
  private final case class Field(name: String) extends Segment
  private final case class Index(value: Long) extends Segment

  val root: JsonPath = JsonPath(Vector.empty)
