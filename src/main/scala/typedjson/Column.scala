package typedjson

import io.circe.{Json, JsonObject}
import typedjson.JsonError.InvalidField

final class Column[A] private (
    val name: String,
    val required: Boolean,
    private[typedjson] val decodeValue: (JsonObject, Int) => Either[Vector[JsonError], A]
)

object Column:
  def required[A](name: String)(using codec: JsonCodec[A]): Column[A] =
    new Column(
      name,
      required = true,
      (obj, record) =>
        obj(name) match
          case None =>
            Left(Vector(InvalidField(record, name, None, "required field is missing")))
          case Some(value) if value.isNull =>
            Left(Vector(InvalidField(record, name, Some(value), "required field cannot be null")))
          case Some(value) =>
            codec.decode(value).left.map(reason =>
              Vector(InvalidField(record, name, Some(value), reason))
            )
    )

  def optional[A](name: String)(using codec: JsonCodec[A]): Column[Option[A]] =
    new Column(
      name,
      required = false,
      (obj, record) =>
        obj(name) match
          case None                                => Right(None)
          case Some(value) if value.isNull          => Right(None)
          case Some(value) =>
            codec.decode(value).map(Some(_)).left.map(reason =>
              Vector(InvalidField(record, name, Some(value), reason))
            )
    )
