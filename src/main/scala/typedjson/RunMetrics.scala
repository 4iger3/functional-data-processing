package typedjson

import typedjson.JsonError.*

final case class RunMetrics(
    records: Long = 0L,
    validRecords: Long = 0L,
    errors: Long = 0L,
    warnings: Long = 0L,
    errorCounts: Map[String, Long] = Map.empty,
    elapsedNanos: Long = 0L
):
  def record(result: DecodeResult[?]): RunMetrics =
    val errorsForRecord = result.value.left.toOption.getOrElse(Vector.empty)
    val updatedCounts = errorsForRecord.foldLeft(errorCounts) { (counts, error) =>
      val name = error match
        case _: MissingField     => "MissingField"
        case _: WrongType        => "WrongType"
        case _: ValidationFailed => "ValidationFailed"
        case _: OutOfRange       => "OutOfRange"
        case _: MalformedJson    => "MalformedJson"
        case _: ExpectedArray    => "ExpectedArray"
        case _: ExpectedObject   => "ExpectedObject"
      counts.updated(name, counts.getOrElse(name, 0L) + 1L)
    }
    copy(
      records = records + 1,
      validRecords = validRecords + (if errorsForRecord.isEmpty then 1 else 0),
      errors = errors + errorsForRecord.size,
      warnings = warnings + result.warnings.size,
      errorCounts = updatedCounts
    )

object RunMetrics:
  def summaryJson(metrics: RunMetrics): io.circe.Json =
    import io.circe.Json
    Json.obj(
      "records" -> Json.fromLong(metrics.records),
      "validRecords" -> Json.fromLong(metrics.validRecords),
      "errors" -> Json.fromLong(metrics.errors),
      "warnings" -> Json.fromLong(metrics.warnings),
      "elapsedNanos" -> Json.fromLong(metrics.elapsedNanos),
      "errorCounts" -> Json.obj(
        metrics.errorCounts.toSeq.map { case (name, count) =>
          name -> Json.fromLong(count)
        }*
      )
    )
