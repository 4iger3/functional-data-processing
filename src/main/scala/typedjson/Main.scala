package typedjson

import cats.effect.{ExitCode, IO}
import cats.syntax.all.*
import com.monovore.decline.*
import com.monovore.decline.effect.CommandIOApp
import io.circe.Json
import java.nio.file.{Path, Paths}

object Main extends CommandIOApp(
      name = "typed-json",
      header = "Validate and process transaction JSON Lines"
    ):
  private final case class FileOptions(
      file: String,
      config: Option[String],
      format: String,
      quiet: Boolean,
      strict: Boolean
  )

  private val fileOptions: Opts[FileOptions] =
    (
      Opts.argument[String](metavar = "FILE"),
      Opts.option[String]("config", help = "HOCON configuration file").orNone,
      Opts.option[String]("format", help = "Output format: text or json").withDefault("text"),
      Opts.flag("quiet", help = "Suppress successful output and warnings").orFalse,
      Opts.flag("strict", help = "Treat warnings as failures").orFalse
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
    val sample = Command("sample", "Print a valid JSON Lines sample")(
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
    else
      IO.fromEither(AppConfig.load(options.config).leftMap(new IllegalArgumentException(_)))
        .flatMap { config =>
          StreamingRunner
            .run(
              Paths.get(options.file),
              config,
              mode,
              options.format.toLowerCase,
              options.quiet,
              strict = options.strict || config.strict
            )
            .map {
              case Right(_) => ExitCode.Success
              case Left(message) =>
                if !options.quiet then System.err.println(s"Error: $message")
                ExitCode.Error
            }
        }
        .handleErrorWith { error =>
          val config = AppConfig.load(options.config).toOption
          config.filter(value => StructuredLog.enabled(value.logLevel, "error")).foreach { _ =>
            StructuredLog.event(
              "error",
              "command_failed",
              "command" -> Json.fromString(modeName(mode)),
              "message" -> Json.fromString(error.getMessage)
            )
          }
          IO(System.err.println(s"Error: ${error.getMessage}")).as(ExitCode.Error)
        }

  private def runSchema(format: String, quiet: Boolean): IO[ExitCode] =
    if quiet then IO.pure(ExitCode.Success)
    else if !Set("text", "json").contains(format.toLowerCase) then
      IO(System.err.println("Error: --format must be either 'text' or 'json'"))
        .as(ExitCode(2))
    else if format.equalsIgnoreCase("json") then
      IO.println(schemaJson.noSpaces).as(ExitCode.Success)
    else
      IO.println(
        """Transaction fields:
          |- date: required ISO-8601 date string
          |- category: required non-empty string
          |- amount: required number in [0, 1000000]; note required when amount >= 1000
          |- note: optional string (deprecated alias: memo)
          |Unknown fields are ignored with warnings.""".stripMargin
      ).as(ExitCode.Success)

  private def runSample(quiet: Boolean): IO[ExitCode] =
    if quiet then IO.pure(ExitCode.Success)
    else
      IO.println("""{"date":"2026-01-03","category":"food","amount":12.5,"note":"lunch"}""") *>
        IO.println("""{"date":"2026-01-04","category":"travel","amount":30.0}""")
          .as(ExitCode.Success)

  private def runExplain(field: String, quiet: Boolean): IO[ExitCode] =
    val explanation = field match
      case "date" =>
        Some("Required ISO-8601 date string; invalid calendar dates are rejected.")
      case "category" =>
        Some("Required non-empty string. Whitespace-only values are rejected.")
      case "amount" =>
        Some("Required JSON number in [0, 1000000]. A note is required for amounts >= 1000.")
      case "note" =>
        Some("Optional string. Missing and null values decode as None; 'memo' is deprecated.")
      case "memo" =>
        Some("Deprecated alias for 'note'. It is accepted with a migration warning.")
      case _ => None
    if quiet then IO.pure(ExitCode.Success)
    else
      explanation match
        case Some(text) => IO.println(s"$field: $text").as(ExitCode.Success)
        case None =>
          IO(System.err.println(s"Error: unknown transaction field '$field'"))
            .as(ExitCode(2))

  private val schemaJson: Json =
    Json.obj(
      "type" -> Json.fromString("array"),
      "items" -> Json.obj(
        "type" -> Json.fromString("object"),
        "required" -> Json.arr(
          Json.fromString("date"),
          Json.fromString("category"),
          Json.fromString("amount")
        ),
        "properties" -> Json.obj(
          "date" -> Json.obj("type" -> Json.fromString("string"), "format" -> Json.fromString("date")),
          "category" -> Json.obj(
            "type" -> Json.fromString("string"),
            "minLength" -> Json.fromInt(1)
          ),
          "amount" -> Json.obj(
            "type" -> Json.fromString("number"),
            "minimum" -> Json.fromInt(0),
            "maximum" -> Json.fromInt(1000000)
          ),
          "note" -> Json.obj("type" -> Json.fromString("string"), "nullable" -> Json.True)
        )
      )
    )

  private def modeName(mode: ProcessingMode): String =
    mode match
      case ProcessingMode.Validate => "validate"
      case ProcessingMode.Parse    => "parse"
