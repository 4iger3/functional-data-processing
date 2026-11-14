package typedjson

import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import com.monovore.decline.*
import com.monovore.decline.effect.CommandIOApp
import io.circe.Json
import java.nio.file.Paths

object Main extends CommandIOApp(
      name = "typed-json",
      header = "Validate and process transaction JSON Lines"
    ):
  private final case class FileOptions(
      file: String,
      config: Option[String],
      format: String,
      quiet: Boolean,
      strict: Boolean,
      maxErrors: Option[Int],
      maxRecordBytes: Option[Int]
  )

  private val fileOptions: Opts[FileOptions] =
    (
      Opts.argument[String](metavar = "FILE"),
      Opts.option[String]("config", help = "HOCON configuration file").orNone,
      Opts.option[String]("format", help = "Output format: text or json").withDefault("text"),
      Opts.flag("quiet", help = "Suppress successful output and warnings").orFalse,
      Opts.flag("strict", help = "Treat warnings as failures").orFalse,
      Opts.option[Int]("max-errors", help = "Maximum number of error details to print").orNone,
      Opts.option[Int]("max-record-bytes", help = "Maximum byte size of a JSON Lines record").orNone
    ).mapN(FileOptions.apply)

  override def main: Opts[IO[ExitCode]] =
    val validate = Command("validate", "Validate a JSON Lines file")(
      fileOptions.map(options => runFile(options, ProcessingMode.Validate))
    )
    val parse = Command("parse", "Validate and emit normalized transactions")(
      fileOptions.map(options => runFile(options, ProcessingMode.Parse))
    )
    val schema = Command("schema", "Print the transaction schema")(
      (
        Opts.option[String]("format", help = "Output format: text or json").withDefault("text"),
        Opts.flag("quiet", help = "Suppress output").orFalse
      ).mapN(runSchema)
    )
    val sample = Command("sample", "Print valid JSON Lines records")(
      Opts.flag("quiet", help = "Suppress output").orFalse.map(runSample)
    )
    val explain = Command("explain", "Explain a transaction field")(
      (
        Opts.argument[String](metavar = "FIELD"),
        Opts.flag("quiet", help = "Suppress output").orFalse
      ).mapN(runExplain)
    )

    Opts.subcommand(validate)
      .orElse(Opts.subcommand(parse))
      .orElse(Opts.subcommand(schema))
      .orElse(Opts.subcommand(sample))
      .orElse(Opts.subcommand(explain))

  private def runFile(options: FileOptions, mode: ProcessingMode): IO[ExitCode] =
    if !Set("text", "json").contains(options.format.toLowerCase) then
      IO(System.err.println("Error: --format must be either 'text' or 'json'"))
        .as(ExitCode(2))
    else if options.maxErrors.exists(_ < 1) then
      IO(System.err.println("Error: --max-errors must be at least 1"))
        .as(ExitCode(2))
    else if options.maxRecordBytes.exists(_ < 1) then
      IO(System.err.println("Error: --max-record-bytes must be at least 1"))
        .as(ExitCode(2))
    else
      IO.fromEither(AppConfig.load(options.config).leftMap(new IllegalArgumentException(_)))
        .flatMap { config =>
          val effectiveConfig = config.copy(
            maxErrors = options.maxErrors.getOrElse(config.maxErrors),
            maxRecordBytes = options.maxRecordBytes.getOrElse(config.maxRecordBytes)
          )
          StreamingRunner
            .run(
              Paths.get(options.file),
              effectiveConfig,
              mode,
              options.format.toLowerCase,
              options.quiet,
              strict = options.strict || effectiveConfig.strict,
              renderJson = StreamingJson.renderTransaction,
              renderText = transaction =>
                s"${transaction.date} ${transaction.category.value} ${transaction.amount}"
            )(using Transaction.schema.codec)
            .map {
              case Right(_) => ExitCode.Success
              case Left(message) =>
                if !options.quiet then System.err.println(s"Error: $message")
                ExitCode.Error
            }
        }
        .handleErrorWith { error =>
          val log = AppConfig.load(options.config) match
            case Right(config) if StructuredLog.enabled(config.logLevel, "error") =>
              IO.delay(
                StructuredLog.event(
                  "error",
                  "command_failed",
                  "command" -> Json.fromString(modeName(mode)),
                  "message" -> Json.fromString(error.getMessage)
                )
              )
            case _ => IO.unit
          log *> IO(System.err.println(s"Error: ${error.getMessage}")).as(ExitCode.Error)
        }

  private def runSchema(format: String, quiet: Boolean): IO[ExitCode] =
    if !Set("text", "json").contains(format.toLowerCase) then
      IO(System.err.println("Error: --format must be either 'text' or 'json'"))
        .as(ExitCode(2))
    else if quiet then IO.pure(ExitCode.Success)
    else if format.equalsIgnoreCase("json") then
      IO.println(schemaJson.noSpaces).as(ExitCode.Success)
    else
      val fieldLines = Transaction.schema.columns.map { column =>
        val requirement = if column.required then "required" else "optional"
        val description =
          column.jsonSchema.hcursor.get[String]("description").toOption.map(s => s": $s").getOrElse("")
        val aliases =
          if column.deprecatedAliases.isEmpty then ""
          else s" (deprecated alias: ${column.deprecatedAliases.mkString(", ")})"
        s"- ${column.name}: $requirement$description$aliases"
      }
      val ruleLines = Transaction.schema.validationRules.map(rule => s"- Cross-field rule: $rule")
      IO.println(
        (Vector("Transaction JSON Lines record fields:") ++ fieldLines ++ ruleLines :+
          "Unknown fields are ignored with warnings.").mkString("\n")
      ).as(ExitCode.Success)

  private def runSample(quiet: Boolean): IO[ExitCode] =
    if quiet then IO.pure(ExitCode.Success)
    else
      IO.println("""{"date":"2026-01-03","category":"food","amount":12.5,"note":"lunch"}""") *>
        IO.println("""{"date":"2026-01-04","category":"travel","amount":30.0}""")
          .as(ExitCode.Success)

  private def runExplain(field: String, quiet: Boolean): IO[ExitCode] =
    val explanation = Transaction.schema.columns
      .find(column => column.name == field || column.deprecatedAliases.contains(field))
      .map { column =>
        if column.deprecatedAliases.contains(field) then
          s"Deprecated alias for '${column.name}'. It is accepted with a migration warning."
        else
          val requirement = if column.required then "Required" else "Optional"
          val description =
            column.jsonSchema.hcursor.get[String]("description").toOption.getOrElse("")
          s"$requirement. $description"
      }
    explanation match
      case Some(_) if quiet => IO.pure(ExitCode.Success)
      case Some(text)       => IO.println(s"$field: $text").as(ExitCode.Success)
      case None =>
        IO(System.err.println(s"Error: unknown transaction field '$field'"))
          .as(ExitCode(2))

  private val schemaJson: Json = Transaction.schema.jsonSchema

  private def modeName(mode: ProcessingMode): String =
    mode match
      case ProcessingMode.Validate => "validate"
      case ProcessingMode.Parse    => "parse"
