package typedjson

import cats.effect.IO
import cats.syntax.all.*
import fs2.Stream
import io.circe.Json
import java.nio.file.Path

enum ProcessingMode:
  case Validate, Parse

object StreamingRunner:
  private[typedjson] final case class DiagnosticState(
      errorsPrinted: Int = 0,
      limitNoticePrinted: Boolean = false
  )

  private[typedjson] final case class DiagnosticPlan(
      state: DiagnosticState,
      messages: Vector[(String, String)]
  )

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
        .evalMapAccumulate(DiagnosticState()) { (state, record) =>
          emitRecord(record, mode, format, quiet, config.maxErrors, state)
        }
        .map(_._2)
        .compile
        .fold(initial) { (metrics, record) => metrics.record(record.result) }
      finished <- IO.monotonic
      metrics = result.copy(elapsedNanos = (finished - started).toNanos)
      _ <- emitSummary(metrics, mode, format, quiet)
      _ <- logCompletion(metrics, mode, strict, config, quiet)
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
      maxErrors: Int,
      state: DiagnosticState
  ): IO[(DiagnosticState, StreamRecord)] =
    val plan = diagnosticPlan(record, maxErrors, quiet, state)
    val diagnostics = traverseVector_(plan.messages) { case (level, message) =>
      IO(System.err.println(s"$level: $message"))
    }

    val output = (mode, record.result.value) match
      case (ProcessingMode.Parse, Right(transaction)) if !quiet =>
        val json = StreamingJson.renderTransaction(transaction)
        val rendered = if format == "json" then json.noSpaces else
          s"${transaction.date} ${transaction.category.value} ${transaction.amount}"
        IO.println(rendered)
      case _ => IO.unit

    (diagnostics *> output).as((plan.state, record))

  private[typedjson] def diagnosticPlan(
      record: StreamRecord,
      maxErrors: Int,
      quiet: Boolean,
      state: DiagnosticState
  ): DiagnosticPlan =
    val errors = record.result.value.left.toOption.getOrElse(Vector.empty)
    val remaining = math.max(0, maxErrors - state.errorsPrinted)
    val visibleErrors = errors.take(remaining)
    val suppressed = errors.size > visibleErrors.size
    val warnings =
      if quiet then Vector.empty
      else record.result.warnings.map(warning => "Warning" -> warning.message)
    val errorMessages = visibleErrors.map(error => "Error" -> error.message)
    val showLimitNotice = suppressed && !state.limitNoticePrinted
    val notice =
      if showLimitNotice && !quiet then
        Vector("Warning" -> s"Further error diagnostics suppressed after $maxErrors errors.")
      else Vector.empty
    DiagnosticPlan(
      DiagnosticState(
        errorsPrinted = state.errorsPrinted + visibleErrors.size,
        limitNoticePrinted = state.limitNoticePrinted || showLimitNotice
      ),
      warnings ++ errorMessages ++ notice
    )

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
      mode: ProcessingMode,
      strict: Boolean,
      config: AppConfig,
      quiet: Boolean
  ): IO[Unit] =
    val failed = metrics.errors > 0 || (strict && metrics.warnings > 0)
    val level = if failed then "error" else "info"
    if quiet && level == "info" || !StructuredLog.enabled(config.logLevel, level) then IO.unit
    else
      val fields = Vector(
        "command" -> Json.fromString(
          mode match
            case ProcessingMode.Validate => "validate"
            case ProcessingMode.Parse    => "parse"
        ),
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
