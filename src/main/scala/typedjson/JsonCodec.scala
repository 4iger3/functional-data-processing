package typedjson

import io.circe.Json
import java.time.LocalDate
import java.time.format.DateTimeParseException
import typedjson.JsonError.*

trait JsonCodec[A]:
  def decode(value: Json, path: JsonPath): DecodeResult[A]

object JsonCodec:
  given JsonCodec[String] with
    def decode(value: Json, path: JsonPath): DecodeResult[String] =
      value.asString match
        case Some(text) => DecodeResult.success(text)
        case None       => DecodeResult.failure(WrongType(path, "a string", jsonType(value)))

  given optionCodec[A](using codec: JsonCodec[A]): JsonCodec[Option[A]] with
    def decode(value: Json, path: JsonPath): DecodeResult[Option[A]] =
      if value.isNull then DecodeResult.success(None)
      else codec.decode(value, path).map(Some(_))

  given listCodec[A](using codec: JsonCodec[A]): JsonCodec[List[A]] with
    def decode(value: Json, path: JsonPath): DecodeResult[List[A]] =
      value.asArray match
        case None =>
          DecodeResult.failure(WrongType(path, "an array", jsonType(value)))
        case Some(values) =>
          val decoded = values.zipWithIndex.map { case (item, index) =>
            codec.decode(item, path.index(index))
          }
          val errors = decoded.flatMap(_.value.left.toOption.getOrElse(Vector.empty))
          val warnings = decoded.flatMap(_.warnings)
          val result =
            if errors.nonEmpty then Left(errors)
            else Right(decoded.flatMap(_.value.toOption).toList)
          DecodeResult(result, warnings)

  given JsonCodec[BigDecimal] with
    def decode(value: Json, path: JsonPath): DecodeResult[BigDecimal] =
      value.asNumber.flatMap(_.toBigDecimal) match
        case Some(number) => DecodeResult.success(number)
        case None         => DecodeResult.failure(WrongType(path, "a number", jsonType(value)))

  given JsonCodec[LocalDate] with
    def decode(value: Json, path: JsonPath): DecodeResult[LocalDate] =
      value.asString match
        case None => DecodeResult.failure(WrongType(path, "a date string", jsonType(value)))
        case Some(text) =>
          try DecodeResult.success(LocalDate.parse(text))
          catch
            case _: DateTimeParseException =>
              DecodeResult.failure(
                ValidationFailed(path, "expected a date in YYYY-MM-DD format")
              )

  private[typedjson] def jsonType(value: Json): String =
    if value.isNull then "null"
    else if value.isString then "a string"
    else if value.isNumber then "a number"
    else if value.isBoolean then "a boolean"
    else if value.isArray then "an array"
    else "an object"
