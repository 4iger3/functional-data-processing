package typedjson

import io.circe.parser.parse
import typedjson.JsonError.*

object JsonData:
  def parseTransactions(
      input: String
  ): Either[Vector[JsonError], Vector[Transaction]] =
    parse(input)
      .left
      .map(error => Vector(MalformedJson(error.message)))
      .flatMap { json =>
        json.asArray match
          case None => Left(Vector(ExpectedArray()))
          case Some(records) =>
            val decoded = records.zipWithIndex.map { case (record, index) =>
              record.asObject match
                case None => Left(Vector(ExpectedObject(index + 1)))
                case Some(value) => Transaction.schema.decodeRecord(value, index + 1)
            }
            val errors = decoded.collect { case Left(recordErrors) => recordErrors }.flatten
            if errors.nonEmpty then Left(errors)
            else Right(decoded.collect { case Right(transaction) => transaction })
      }
