package typedjson

import io.circe.{Json, JsonObject}
import typedjson.JsonError.*

final class Column[A] private (
    val name: String,
    val required: Boolean,
    val deprecatedAliases: Vector[String],
    private val decodeField: (Option[Json], JsonPath) => DecodeResult[A]
):
  private[typedjson] def decodeValue(obj: JsonObject, path: JsonPath): DecodeResult[A] =
    val aliasPresent = deprecatedAliases.filter(obj.contains)
    val selectedName =
      if obj.contains(name) then Some(name)
      else aliasPresent.headOption
    val fieldPath = selectedName.fold(path.field(name))(path.field)
    val decoded = decodeField(selectedName.flatMap(obj.apply), fieldPath)
    val aliasWarnings = aliasPresent.map { alias =>
      JsonWarning.deprecatedField(path.field(alias), name)
    }
    DecodeResult(decoded.value, aliasWarnings ++ decoded.warnings)

  def deprecatedAlias(alias: String): Column[A] =
    require(alias.nonEmpty, "deprecated alias must not be empty")
    require(alias != name, "deprecated alias must differ from the current field name")
    require(!deprecatedAliases.contains(alias), s"deprecated alias '$alias' is already defined")
    new Column(name, required, deprecatedAliases :+ alias, decodeField)

  def validateRange(minimum: A, maximum: A)(using ordering: Ordering[A]): Column[A] =
    require(ordering.lteq(minimum, maximum), "minimum must not be greater than maximum")
    new Column(
      name,
      required,
      deprecatedAliases,
      (raw, path) =>
        val decoded = decodeField(raw, path)
        decoded.value match
          case Right(value)
              if !ordering.gteq(value, minimum) || !ordering.lteq(value, maximum) =>
            val error = OutOfRange(
              path,
              minimum.toString,
              maximum.toString,
              value.toString
            )
            DecodeResult(Left(Vector(error)), decoded.warnings)
          case _ => decoded
    )

object Column:
  def required[A](name: String)(using codec: JsonCodec[A]): Column[A] =
    new Column(
      name,
      required = true,
      deprecatedAliases = Vector.empty,
      (raw, path) =>
        raw match
          case None => DecodeResult.failure(MissingField(path))
          case Some(value) if value.isNull =>
            DecodeResult.failure(WrongType(path, "a non-null value", "null"))
          case Some(value) => codec.decode(value, path)
    )

  def optional[A](name: String)(using codec: JsonCodec[A]): Column[Option[A]] =
    new Column(
      name,
      required = false,
      deprecatedAliases = Vector.empty,
      (raw, path) =>
        raw match
          case None        => DecodeResult.success(None)
          case Some(value) => summon[JsonCodec[Option[A]]].decode(value, path)
    )
