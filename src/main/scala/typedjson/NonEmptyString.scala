package typedjson

import io.circe.Json
import typedjson.JsonError.{ValidationFailed, WrongType}

opaque type NonEmptyString = String

object NonEmptyString:
  def from(value: String): Either[String, NonEmptyString] =
    Either.cond(value.trim.nonEmpty, value, "value must not be empty")

  extension (value: NonEmptyString)
    def value: String = value

  given JsonCodec[NonEmptyString] with
    def decode(json: Json, path: JsonPath): DecodeResult[NonEmptyString] =
      json.asString match
        case None =>
          DecodeResult.failure(WrongType(path, "a string", JsonCodec.jsonType(json)))
        case Some(text) =>
          from(text) match
            case Right(nonEmpty) => DecodeResult.success(nonEmpty)
            case Left(reason) =>
              DecodeResult.failure(ValidationFailed(path, reason))
