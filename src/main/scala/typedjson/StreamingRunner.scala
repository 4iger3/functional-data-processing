package typedjson

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import io.circe.Json
import java.nio.file.Path

enum ProcessingMode:
  case Validate, Parse

object StreamingRunner:
  def run(
      path: Path,
      config: AppConfig,
      mode: ProcessingMode,
      format: String,
      quiet: Boolean,
      strict: Boolean
  ): IO[Either[String, RunMetrics]] =
    for
      started <- IO.monotonic
      initial = RunMetrics()
      result <- StreamingJson
        .records(path, config)
        .evalMap(record => emitRecord(record, mode, format, quiet, config.maxErrors))
        .compile
        .fold(initial) { (metrics, record) => metrics.record(record.result) }
      finished <- IO.monotonic
      metrics = result.copy(elapsedNanos = (finished - started).toNanos)
      _ <- emitSummary(metrics, mode, format, quiet)
      _ <- logCompletion(metrics, strict, config, quiet)
      outcome =
        if metrics.errors > 0 then Left(s"Found ${metrics.errors} decoding errors.")
        else if strict && metrics.warnings > 0 then
          Left(s"Strict mode rejects ${metrics.warnings} warnings.")
        else Right(metrics)
    yield outcome

  private def emitRecord(
      record: StreamRecord,
      mode: ProcessingMode,
      format: String,
      quiet: Boolean,
      maxErrors: Int
  ): IO[StreamRecord] =
    val errors = record.result.value.left.toOption.getOrElse(Vector.empty)
    val warningMessages = record.result.warnings
    val diagnostics =
      if record.line > maxErrors then IO.unit
      else
        traverseVector_(warningMessages)(warning =>
          IO(System.err.println(s"Warning: ${warning.message}")).whenA(!quiet)
        ) *> traverseVector_(errors)(error =>
          IO(System.err.println(s"Error: ${error.message}"))
        )

    val output = (mode, record.result.value) match
      case (ProcessingMode.Parse, Right(transaction)) if !quiet =>
        val json = StreamingJson.renderTransaction(transaction)
        val rendered = if format == "json" then json.noSpaces else
          s"${transaction.date} ${transaction.category.value} ${transaction.amount}"
        IO.println(rendered)
      case _ => IO.unit

    diagnostics *> output.as(record)

  private def emitSummary(
      metrics: RunMetrics,
      mode: ProcessingMode,
      format: String,
      quiet: Boolean
  ): IO[Unit] =
    if quiet || mode == ProcessingMode.Parse then IO.unit
    else if format == "json" then IO.println(RunMetrics.summaryJson(metrics).noSpaces)
    else
      IO.println(
        s"Records: ${metrics.records}, valid: ${metrics.validRecords}, " +
          s"errors: ${metrics.errors}, warnings: ${metrics.warnings}, " +
          f"elapsed: ${metrics.elapsedNanos / 1000000.0}%.2f ms"
      )

  private def logCompletion(
      metrics: RunMetrics,
      strict: Boolean,
      config: AppConfig,
      quiet: Boolean
  ): IO[Unit] =
    val failed = metrics.errors > 0 || (strict && metrics.warnings > 0)
    val level = if failed then "error" else "info"
    if quiet && level == "info" || !StructuredLog.enabled(config.logLevel, level) then IO.unit
    else
      val fields = Vector(
        "command" -> Json.fromString("stream"),
        "status" -> Json.fromString(if failed then "failed" else "succeeded"),
        "records" -> Json.fromLong(metrics.records),
        "validRecords" -> Json.fromLong(metrics.validRecords),
        "errors" -> Json.fromLong(metrics.errors),
        "warnings" -> Json.fromLong(metrics.warnings),
        "elapsedNanos" -> Json.fromLong(metrics.elapsedNanos),
        "errorCounts" -> RunMetrics.summaryJson(metrics)
          .hcursor.downField("errorCounts").focus.getOrElse(Json.obj())
      )
      IO.delay(StructuredLog.event(level, "command_completed", fields*))

  private def traverseVector_[A](values: Vector[A])(f: A => IO[Unit]): IO[Unit] =
    values.foldLeft(IO.unit)((effect, value) => effect *> f(value))
