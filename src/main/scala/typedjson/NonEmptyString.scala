package typedjson

import io.circe.Json
import typedjson.JsonError.{ValidationFailed, WrongType}

opaque type NonEmptyString = String

object NonEmptyString:
  private val jsonSchema = Json.obj(
    "type" -> Json.fromString("string"),
    "minLength" -> Json.fromInt(1),
    "pattern" -> Json.fromString("\\S"),
    "description" -> Json.fromString("Must contain at least one non-whitespace character.")
  )

  def from(value: String): Either[String, NonEmptyString] =
    Either.cond(value.trim.nonEmpty, value, "value must not be empty")

  extension (value: NonEmptyString)
    def value: String = value

  given JsonCodec[NonEmptyString] with
    val schema: Json = jsonSchema

    def decode(json: Json, path: JsonPath): DecodeResult[NonEmptyString] =
      json.asString match
        case None =>
          DecodeResult.failure(WrongType(path, "a string", JsonCodec.jsonType(json)))
        case Some(text) =>
          from(text) match
            case Right(nonEmpty) => DecodeResult.success(nonEmpty)
            case Left(reason) =>
              DecodeResult.failure(ValidationFailed(path, reason))
