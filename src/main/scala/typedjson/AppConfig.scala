package typedjson

import com.typesafe.config.ConfigFactory
import java.io.File
import scala.util.control.NonFatal

final case class AppConfig(
    strict: Boolean,
    parallelism: Int,
    streamChunkSize: Int,
    inputChunkSize: Int,
    maxRecordBytes: Int,
    maxErrors: Int,
    logLevel: String
)

object AppConfig:
  private val defaults = AppConfig(
    strict = false,
    parallelism = 4,
    streamChunkSize = 64,
    inputChunkSize = 8192,
    maxRecordBytes = 1024 * 1024,
    maxErrors = 100,
    logLevel = "info"
  )

  def load(configPath: Option[String]): Either[String, AppConfig] =
    try
      val loaded = configPath match
        case Some(path) =>
          ConfigFactory.parseFile(new File(path)).withFallback(ConfigFactory.load())
        case None => ConfigFactory.load()
      val resolved = loaded.resolve()
      val section = resolved.getConfig("typed-json")
      val settings = AppConfig(
        strict = section.getBoolean("strict"),
        parallelism = section.getInt("parallelism"),
        streamChunkSize = section.getInt("stream-chunk-size"),
        inputChunkSize = section.getInt("input-chunk-size"),
        maxRecordBytes = section.getInt("max-record-bytes"),
        maxErrors = section.getInt("max-errors"),
        logLevel = section.getString("log-level").toLowerCase
      )
      validate(settings)
    catch
      case NonFatal(error) =>
        Left(s"Could not load configuration: ${error.getMessage}")

  private def validate(config: AppConfig): Either[String, AppConfig] =
    if config.parallelism < 1 then Left("typed-json.parallelism must be at least 1")
    else if config.streamChunkSize < 1 then Left("typed-json.stream-chunk-size must be at least 1")
    else if config.inputChunkSize < 1 then Left("typed-json.input-chunk-size must be at least 1")
    else if config.maxRecordBytes < 1 then Left("typed-json.max-record-bytes must be at least 1")
    else if config.maxErrors < 1 then Left("typed-json.max-errors must be at least 1")
    else if !Set("debug", "info", "warn", "error", "off").contains(config.logLevel) then
      Left("typed-json.log-level must be debug, info, warn, error, or off")
    else Right(config)
