package typedjson

import io.circe.parser.parse
import typedjson.JsonError.*

object JsonData:
  def decodeTransaction(
      json: io.circe.Json,
      path: JsonPath
  ): DecodeResult[Transaction] =
    Transaction.schema.decodeJson(json, path)

  def parseTransactions(
      input: String
  ): DecodeResult[Vector[Transaction]] =
    parse(input) match
      case Left(error) =>
        DecodeResult.failure(MalformedJson(JsonPath.root, error.message))
      case Right(json) =>
        json.asArray match
          case None =>
            DecodeResult.failure(ExpectedArray(JsonPath.root))
          case Some(records) =>
            val decoded = records.zipWithIndex.map { case (record, index) =>
              val path = JsonPath.root.index(index)
              record.asObject match
                case None => DecodeResult.failure[Transaction](ExpectedObject(path))
                case Some(_) => decodeTransaction(record, path)
            }
            val errors = decoded.flatMap(_.value.left.toOption.getOrElse(Vector.empty))
            val warnings = decoded.flatMap(_.warnings)
            val values =
              if errors.nonEmpty then Left(errors)
              else Right(decoded.flatMap(_.value.toOption))
            DecodeResult(values, warnings)
