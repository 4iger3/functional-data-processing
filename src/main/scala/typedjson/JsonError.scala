package typedjson

import io.circe.Json

sealed trait JsonError:
  def message: String

object JsonError:
  final case class MalformedJson(detail: String) extends JsonError:
    def message: String = s"Malformed JSON: $detail"

  final case class ExpectedArray() extends JsonError:
    def message: String = "Expected the top-level JSON value to be an array."

  final case class ExpectedObject(record: Int) extends JsonError:
    def message: String = s"Record $record: expected a JSON object."

  final case class InvalidField(
      record: Int,
      name: String,
      raw: Option[Json],
      detail: String
  ) extends JsonError:
    def message: String =
      val value = raw.fold("missing value")(json => json.noSpaces)
      s"Record $record, field '$name': $detail ($value)."
