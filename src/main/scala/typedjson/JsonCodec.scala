package typedjson

import io.circe.Json
import java.time.LocalDate
import java.time.format.DateTimeParseException

trait JsonCodec[A]:
  def decode(value: Json): Either[String, A]

object JsonCodec:
  given JsonCodec[String] with
    def decode(value: Json): Either[String, String] =
      value.asString.toRight("expected a JSON string")

  given JsonCodec[BigDecimal] with
    def decode(value: Json): Either[String, BigDecimal] =
      value.asNumber.flatMap(_.toBigDecimal).toRight("expected a JSON number")

  given JsonCodec[LocalDate] with
    def decode(value: Json): Either[String, LocalDate] =
      summon[JsonCodec[String]].decode(value).flatMap { text =>
        try Right(LocalDate.parse(text))
        catch
          case _: DateTimeParseException => Left("expected a date in YYYY-MM-DD format")
      }
